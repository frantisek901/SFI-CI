;;; New updates:
;;; 1) listeners listen to whoever, not only to Indies - everyone has its distance DIST = measure to attractivity and ability to warp social space
;;; 2) Indies divide after each change the PCHANGE (probability of change) by CHANGEFACTOR, it means that after each change it is less and less probable to change

extensions [profiler matrix]
breed [commoners commoner]
breed [indies indie]
breed [boxes box]

globals [
  ;;;; USEFUL GLOBAL VARIABLES AND CONSTANTS
  ;; number of indies
  Ni
  ;; number of all turtles
  Na
  ;; Entropy of individual entropies
  entMean
  ;; Size of box for counting fractal dimension
  boxSize
  ;; Stores (log of 1/boxSize)
  xAxisList
  ;; Stores (log of BOX-COUNT)
  yAxisList

  ;;;;; GLOBAL MONITORS
  ;; H] number of indies with avantgarde attitudes
  aIndies ;; For VELVET version we need only one variable, which will be stored every step.

  ;; I] number of "normal" turtles with avantgarde attitudes
  aComms ;; For VELVET version we need only one variable, which will be stored every step.

  ;; N] number of all turtles with avantgarde attitudes
  aAll ;; For VELVET version we need only one variable, which will be stored every step.
]

turtles-own [
  ;; this is opinion of the agent; here we follow theory:
  ;; attitude is internal state of actor, opinion is expressed attitude,
  ;; that is why we use variable name ATTITUDE and not opinion
  attitude
  ;; attitude to which the turtle was persuaded
  newAttitude
  ;; it is persuasion of the others on the agent
  persuasiveness
  ;; it is how much the others support the agent
  supportiveness
  ;; now, number of media the agent is susceptible to - in previous versions it was list coding respective media
  susceptibility
  ;; variable, where I can store the list of agents in the neghborhood (r=10)
  nei
  ;; variable, where I store the INDIES which agent listens to
  myIndies
  ;; decide to change opinion
  changed?
  ;; indicator of possibility of change - whether something changed in NEI
  chanceOfChange?
  ;; entropy of agent's NEI and listened Indies
  ent
  ;; distance felt by someone who listens
  dist
]

commoners-own [
  listeners
]

indies-own [
  ;; agentset of all listeners
  listeners

  ;; pChange = probability that the Indie changes the value/attitude/opinion
  pChange
]

boxes-own [
  ;; ID, more useful tahn inate WHO
  id
  ;; virtual coordinates in BOXes' array
  xBox
  yBox
  ;; coordinates of area covered by BOX
  yBoxMin
  yBoxMax
  xBoxMin
  xBoxMax
  ;; turtles covered by shape of the BOX
  coveredTurtles
]

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; list of all attitudes which are possible to be used by agents
to-report possibleAttitudes
  report n-values allAttitudes [ [?1] -> ?1 ]
end

to setup
  ;; clearing the world
  clear-all
  ask patches [set pcolor white - 2]

  ;; firstly, setting the random seed and number of attitudes
  if RS <= 0 or RS > 1000 [
    let x random 2147483647
    set RS x
  ]
  random-seed RS
  if allAttitudes < initialAttitudes [set allAttitudes (initialAttitudes)]

  ;; Reseting is of almost same length as reshuffling, so I will just reset it every time
  reset-world

  ;; Initializing
  ask turtles [
    initialize
    set dist 1 / random-exponential distanceOfListenedIndies
  ]

  Ifelse %Indies > 0 [
    ;; Intializing INDIES at the end of setup
    ;; We need to do it here, after initialization,
    ;; cos' initialization turns COMMONER into INDIES
    set-indies

    ;; change distances of Indies and Commoners
    change-distances

    ;; Setting the list of Indies susceptibility
    ask turtles [set-susceptibility-poisson]

    ;; Defining agentsets of listeners
    ;ask indies [set listeners no-turtles]
    ask turtles [
      set myIndies no-turtles
      set listeners no-turtles
      setup-listeners
    ]
  ][
    ask turtles[
      set myIndies no-turtles
      set listeners no-turtles
    ]
  ]

  ;; For sure, we open all agents towards the change
  ask turtles [set chanceOfChange? true]

  ;; Counting initial entropy before initializing graphs
  if entropyMeasure != "none" [ask turtles [count-ind-entropy]]

  ;; Definite end of setup - reseting ticks and graphs
  clear-all-plots
  reset-ticks

  ;; Recounting and storing agregated variables
  recount-agregated-variables
  if record? [savingData-preparingFile]
end



;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; SETUP procedures ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;
to reset-world
  resize-world 0 lengthOfWorld 0 lengthOfWorld
  set-patch-size 460 / (lengthOfWorld + 1)
  ask patches [set pcolor white - 2]

  ;; Setting number of turtles
  set Na (count patches)
  set Na (Na * densityOfAgents / 100)

  ;; initialize the first turtles
  ifelse preferentialOmitting? and (densityOfAgents < 100) [
    ask patches [
      sprout-commoners 1 [
        set shape "square"
        set attitude random initialAttitudes
        set newAttitude attitude
        set ent 0
      ]
    ]
    ask n-of (100 - densityOfAgents) commoners [die]
    while [count commoners > Na] [
      ask one-of commoners with [count turtles-on neighbors < 8] [die]
    ]
  ][
    ask n-of Na patches [
      sprout-commoners 1 [
        set shape "square"
        set attitude random initialAttitudes
        set newAttitude attitude
      ]
    ]
  ]

  ;; Defining NEI and neiMedia for MEDIA and nonMediaAgents
  ask turtles [set nei commoners in-radius (10 - distanceToMyself)]  ;; now we set "nei", i.e. neighbourhood
end


to change-distances
  ;; Acommodating slider SAMPLESIZE - if there are less Comms tahn SAMPLESIZE we have to set slider down
  if count commoners < sampleSize [set sampleSize count commoners]

  ;; Indies will here change their DIST parameter with Comms
  ask indies [
    ;show dist

    ;; Indie gets the sample of Comms and the best Comm
    let sample (n-of sampleSize commoners)
    let best sample with-min [dist]
    ;show (word best ":" [dist] of best)

    ;; Changing with the best
    if dist > first [dist] of best [
      let x dist
      set dist (first [dist] of best)
      ask best [set dist x]
    ]
    ;show dist
  ]
end


to setup-listeners
  ask myIndies [set listeners (turtle-set myself listeners)]
end


to set-susceptibility-poisson
  ;; Firstly we set MAX-LISTENERS: number of INDIES that might be listened by the acting agent
  let maxListeners count (other turtles with [distance myself > dist])   ;; yes, OTHER! For non-INDIES it does not hurt and INDIES choose different INDIES then themselves

  ;; We define randomly number of INDIES the agent is susceptible to...
  set susceptibility random-poisson listenedIndiesOnAverage
  while [susceptibility > maxListeners] [set susceptibility random-poisson listenedIndiesOnAverage]

  ;; ...and then we create agent set randomly chosen from INDIES breed and of size of SUSCEPTIBILITY
  ;; There is no point in listening to INDIES who are closer than DISTANCE-OF-LISTENED-INDIES,
  ;; because these INDIES are naturally part of NEIGHBORHOOD and are stronger than listened INDIES,
  ;; so we ask only INDIES further than DISTANCE-OF-LISTENED-INDIES, just in case this limit is further than boundary of NEIGHBORHOOD
  ;; we check it and in this case we find INDIES for listening immediately behind the NEIGHBORHOOD's boundary
  set myIndies n-of susceptibility other turtles with [distance myself > dist]
  ;; yes, OTHER! For non-INDIES it does not hurt and INDIES choose different INDIES then themselves

  ;; It might happen the one of MY-INDIES is also part of NEI, so we have to check it and pull the listened INDIES out of NEI
  ask myIndies [
    if member? self [nei] of myself [
      let newNei (other [nei] of myself)
      ask myself [set nei [newNei] of myself]
    ]
  ]
end


to set-indies
  set Ni (Na * %Indies / 100)
  ask n-of Ni turtles [
    set breed indies
    set shape "star"
    set heading 0
    set size 2
    set listeners no-turtles
    set persuasiveness (minIndiesPersuasiveness + random (100.1 - minIndiesPersuasiveness))
    set supportiveness (minIndiesSupportiveness + random (100.1 - minIndiesSupportiveness))
    set pChange probOfIndieChange
  ]
end


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

to go
  ;; the first thing: (a) DSIT - dissolve persuasion
  ask turtles [

    ;; at the begining we check whether there is the possibility of change of pressure -
    ;; - if there is no change in NEI or MYINDIES (listened INDIES), there is no possibility of pressure change
    if chanceOfChange? [

      ;; The first of all - I have to set chanceOfChange? FALSE, later it would be hard to distinguish if or not unset this indicator.
      set chanceOfChange? false

      ;; In case there is some change in NEI or media (indicated earlier by CHANCEOFCHANGE?) we have to compare supporting and persuading pressures
      do-persuasion

      ;; the second thing: (b) DSITimp - updating turtles with changed attitude
      if changed? [update-turtles]
    ]
  ]


  ;; the third thing: (c) IIC - letting indies to change their attitude
  ask indies [if (probOfIndieChange > random-float 1) [initialize-indies]]

  ;; recounting and saving agregated measures
  recount-agregated-variables
  if record? [savingData-regularRecord]
  if entropyMeasure != "none" [
    ask turtles [count-ind-entropy]
    set entMean find-entMean
  ]

  ;; Closing turn/simulation
  tick
  if ticks = stopTicks [
    file-close
    stop
  ]
end


;;;;;;;;;;;;;;;;;;;;;;;
;;;; GO procedures ;;;;
;;;;;;;;;;;;;;;;;;;;;;;
to do-persuasion
    ;; during GO procedure we
    ;; 1] scan NEI for all existing attitudes,
    ;; 2] then on random basis we make 1to1 comparison
    ;; 3] if some camp suceedes and persuades the turtle, comparisons stop and turtle takes suceeding attitude

    ;; Scaning NEI for supporters and adversaries
    let supporters nei with [attitude = [attitude] of myself]
    let persuasors nei with [attitude != [attitude] of myself]

    ;; Scaning INDIES for support and persuasion
    let suppIndies myIndies with [attitude = [attitude] of myself]
    let persIndies myIndies with [attitude != [attitude] of myself]

    ;; creating list of adversary attitudes - firstly saving all attitudes of PERSUASORS, secondly reducing list from duplicates
    let allPers (turtle-set persIndies persuasors)
    let challengingAttitudes [attitude] of allPers
    set challengingAttitudes remove-duplicates challengingAttitudes
    set challengingAttitudes shuffle challengingAttitudes ;; now is order of comparison random

    ;; In case there are no challengingAttitudes, there is no point in continuing with the procedure,
    ;; i.e. count support
    if length challengingAttitudes = 0 [stop]

    ;; Now we compute supportive force of SUPPORTERS - it will be same for all 1to1 comparisons
    let support 0
    set support find-support (supporters) (suppIndies)

    ;; For comparisons we will use cycle
    while [not changed? and length challengingAttitudes > 0] [

      ;; We compute the power of Persuasion - firstly we take AdversaryAttitude from the list
      let persuasion 0
      let adversaryAttitude first challengingAttitudes
      set challengingAttitudes (remove adversaryAttitude challengingAttitudes) ;; we shorten list of "challengingAttitudes" immediately

      ;; Secondly, we find Persuasors among NEI and INDIES
      let adversaries (persuasors with [attitude = adversaryAttitude])
      let adveIndies (persIndies with [attitude = adversaryAttitude])

      ;; Now, we could compute the power of Persuasion
      set persuasion find-persuasion (adversaries) (adveIndies)

      ;; Finally, we compare power of Persuasion and power of Support and
      ;; in case of Persuasion success we change the attitude towards the Persuasion
      if persuasion > support [ ;;
        set newAttitude adversaryAttitude
        set changed? true
      ]
    ]
end


to update-turtles
  set attitude (newAttitude)
  initialize
end


to initialize
  set persuasiveness random 100.1
  set supportiveness random 100.1
  set color find-color
  set size 1

  ;; reseting change? and chanceOfChange? markers, including NEIs' indicators
  set changed? false        ;; cos' we just have done initialization and we do not want do it again without reason
  set chanceOfChange? true  ;; cos' agent might change to not suitable opinion, so the change is prone to future change
  ask nei [set chanceOfChange? true]      ;; cos' agent's change could lead to change of (at least some) NEIs
  if (breed = indies) [ask listeners [set chanceOfChange? true]]
end


to initialize-indies
  ;; taking new and different attitude from set of all possible attitudes
  set attitude one-of possibleAttitudes

  ;; reseting change? and chanceOfChange? markers, including NEIs' indicators
  set changed? false        ;; cos' we just have done initialization and we do not want do it again without reason
  set chanceOfChange? true  ;; cos' agent might change to not suitable opinion, so the change is prone to future change
  ask nei [set chanceOfChange? true]      ;; cos' turtle's change could lead to change of (at least some) NEIsisteners
  ask listeners [set chanceOfChange? true]      ;; cos' Indie's change could lead to change of (at least some) Indie'sListeners

  ;; set other traits
  set persuasiveness (minIndiesPersuasiveness + random (100.1 - minIndiesPersuasiveness))
  set supportiveness (minIndiesSupportiveness + random (100.1 - minIndiesSupportiveness))
  set color find-color
  set size 2  ;; INDIES standing for their independent opinion will be bigger than INDIES convinced to opinion through the DSIT process
  set pChange (pChange / changeFactor) ;; with every change the next change is less and less possible
end


to recount-agregated-variables
  ;; H] number of indies with avantgarde attitudes
  set aIndies  (count indies with [attitude >= initialAttitudes])

  ;; I] number of "normal" turtles with avantgarde attitudes
  set aComms (count commoners with [attitude >= initialAttitudes])

  ;; N] number of all turtles with avantgarde attitudes
  set aAll (aIndies + aComms)
end


to-report find-support [supporters suppIndies]
  ;; persons
  let N ((count supporters) + (count suppIndies))
  let num 0
  let rank 0
  ask supporters [set num (num + (supportiveness / ((distance myself + distanceToMyself) ^ exponent)))]

  ;; Indies
  ask suppIndies [set num (num + (supportiveness / ((dist + distanceToMyself) ^ exponent)))]

  ;; final reporting
  set num (num / N)
  report (N ^ 0.5) * num
end


to-report find-persuasion [adversaries adveIndies]
  ;; persons
  let N ((count adversaries) + (count adveIndies))
  let num 0
  let rank 0
  ask adversaries [set num (num + (persuasiveness / ((distance myself + distanceToMyself) ^ exponent)))]

  ;; Indies
  ask adveIndies [set num (num + (persuasiveness / ((dist + distanceToMyself) ^ exponent)))]

  ;; final reporting
  set num (num / N)
  report (N ^ 0.5) * num
end


to-report find-color
  report (attitude * 10) + 25
end


to-report exponent
  report 2
end


to savingData-preparingFile
  ;; Firstly, we prepare, fill, and close file of initial attitudes
  ifelse (not file-exists? filenameForRecordingResults) [
    file-open filenameForRecordingResults
    file-print "rs; dli; mli; step; aInd; aCom; aAll"
    file-flush
  ][file-open filenameForRecordingResults]

  ;; Writing data on situation after setup
  file-print (word
      RS ";"
      distanceOfListenedIndies ";"
      listenedIndiesOnAverage ";"
      ticks ";"
      aIndies ";"
      aComms ";"
      aAll)
  file-flush
end


to savingData-regularRecord
  ;; Recording data on continuos situation to file
  file-print (word
      RS ";"
      distanceOfListenedIndies ";"
      listenedIndiesOnAverage ";"
      (ticks + 1) ";"
      aIndies ";"
      aComms ";"
      aAll)
  file-flush
end


;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Entropy procedures ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;
to count-ind-entropy
  ;;;; Firstly, we have to acquire size of power for each attitude, we will go from attitude 0 to attitude = (allAttitudes - 1),
  ;;;; and we store all numbers as a list

  ;; Initialization of variables
  let att 0
  let value 0
  let values []
  let neis no-turtles
  let inds no-turtles

  ;; loop over all attitudes' values
  while [att < allAttitudes] [

    ;; Actualization of variables NEIS and INDS
    set neis nei with [attitude = att]
    set inds myIndies with [attitude = att]

    ;; Several Entropy measures
    if entropyMeasure = "power of opinions" [
      ;; If ATT==ATITUDE of agent, then we find support, if it's different, then we find persuasion
      ;; but we will apply these procedures to same NEIS and INDS
      ifelse att = attitude [
        carefully [set value find-support (neis) (inds)] [set value 0]
      ][
        carefully [set value find-persuasion (neis) (inds)] [set value 0]
      ]
    ]
    if entropyMeasure = "distance of opinions" [
      set value sum [1 / (distance myself + distanceToMyself)] of neis                     ;; Note: Literally, we measure reversed distance, not distance exactly.
      ;print (word "ATT: " att ", NEIs: " value) ;; Visual Code Testing
      set value value + (count inds * (1 / (distanceOfListenedIndies + distanceToMyself)))
      ;print (word "ATT: " att ", NEIs+INDs: " value) ;; Visual Code Testing
    ]
    if entropyMeasure = "count of opinions" [
      set value (count neis + count inds)
    ]

    ;; Storing the VALUE
    set values fput (precision (value) 3) values

    ;; Actualization of ATT
    set att (att + 1)
  ]

  ;;;; Secondly, we count H (entropy)
  ;; Initialization of variables
  let suma (sum values)
  let i 0
  set ent 0
  set value 0

  ;; Counting H through the cycle
  while [i < allAttitudes] [
    ;; Compute VALUE of pi
    set value ((item i values) / suma)

    ;; Computing and summing part of H into ENT
    if value > 0 [set ent ent + (value * log value 2)]

    ;; Actualization of I
    set i (i + 1)
  ]

  ;; Correcting H by -
  set ent precision (-1 * ent) 3
end

to-report find-entMean
  ;; We calculate entropy of individually faced entropies (ENT) in several steps:
  ;; 1) we obtain list of ENTs
  ;; 2) according GRANULARITY we recode them: min = 1, max = 2 ^ GRANULARITY
  ;; 3) we compute H of recoded list

  ;; 1: Getting the list
  let entList ([ent] of turtles)

  ;; 2: Recoding
  ;; a: Preparing needed variables
  let entMax (log allAttitudes 2)
  let grainMax (2 ^ granularity)
  let recodedList []

  ;; b: Recoding itself - going over ENTLIST and transform values and store them to RECODEDLIST
  foreach entList [ x ->
    set recodedList fput ceiling(x / entMax * grainMax) recodedList
  ]

  ;; 3: Computing H
  ;; a: Preparing needed variables
  let values remove-duplicates recodedList
  let fractions []
  let listLength length recodedList
  let xLength 0
  let H 0

  ;; b: Computing fractions of VALUES
  foreach values [ x ->
    set xLength (length remove x recodedList)
    set fractions fput ((listLength - xLength) / listLength) fractions
  ]

  ;; c: Computing H itself
  foreach fractions [ x ->
    if x > 0 [set H (H - (x * log x 2))]
  ]

  ;; 4: Reporting H
  report H
end


;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Fractal procedures ;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;
to count-box-fractal-dimension
  ;; 0) prepare variables
  ;; 1) cover world by boxes - full squares
  ;; 2) let die boxes not covering Indies/Comms with measured attitude
  ;; 3) count and plot numbers
  ;; 4) repeat [1]-[3] with larger size of box
  ;; 5) compute regression
  ;; 6) repeat [1]-[5] for all ATTITUDES

  ;; 0) prepare everything for counting - mainly variables
  set-current-plot "Box Counting Plot"
  clear-plot
  set computedAttitude -1
  ask boxes [die] ;; Note: We need kill all the BOXes not to include their attitude -1 into ATTITUDE-LIST
  let attitudeList sort (remove-duplicates ([attitude] of turtles))


  ;; 6) repeat [1]-[5] for all ATTITUDES
  print "-----------------------------------------------------------"
  foreach attitudeList [ x ->
    ;; Reseting needed variables
    set computedAttitude x
    set xAxisList []
    set yAxisList []
    set boxSize (initialBoxSize - increment)

    ;; 4) Repeating steps [1]-[3]
    repeat repeats [
      ;; updating BOX-SIZE
      set boxSize (boxSize + increment)

      ;; routines
      cover-world-by-boxes
      let-die-not-covering-boxes
      count-and-plot-numbers
    ]

    ;; 5) compute regression
    compute-regression
  ]
  print "-----------------------------------------------------------"

  ;; For smooth continuing we have to kill all BOXes
  ask boxes [die]
end


to compute-regression
    ;; 5) compute regression
    let regression matrix:regress matrix:from-column-list (list yAxisList  xAxisList)   ;using the regression tool from the matrix extension
    let y-intercept item 0 (item 0 regression)                                          ;setting y-intercept and slope (measure of goodness of fit)
    let slope item 1 (item 0 regression)
    let r-square item 0 (item 1 regression)

    ;; Print and set the equation to the appropriate string
    let lin-reg-eq (word (precision slope 3) " * x + " (precision y-intercept 3))
    print (word "Attitude " computedAttitude " at step " ticks ": " lin-reg-eq "; R^2 = " (precision r-square 3))

    ;; Plotting TRENDs
    set-current-plot "Box Counting Plot"
    set-current-plot-pen (word "trend" computedAttitude)
    plot-pen-reset
    auto-plot-off
    plotxy plot-x-min (plot-x-min * slope + y-intercept)
    plot-pen-down
    plotxy plot-x-max (plot-x-max * slope + y-intercept)
    plot-pen-up
end


to cover-world-by-boxes
  ;; 1) cover world by boxes - full squares
  ;; a) computing number of boxes able to cover whole world
  let boxCount (ceiling (world-width / boxSize)) ^ 2 ;; Note: we use CEILING, so some BOXES overlap, but all turtles in the world are covered

  ;; b) creating boxes
  ask boxes [die] ;; Note: Before creating new BOXes we have to kill the old ones
  create-boxes boxCount [
    set shape "square-full"
    set color white
    set size boxSize
    set attitude -1
  ]

  ;; c) spreading the boxes - we will use the WHO variable to determine exact position of BOX
  ;; firstly, we need find lowest WHO of BOXES and set ID
  let firstBox min [who] of boxes
  ask boxes [set id (who - firstBox)]

  ;; secondly, we calculate XCOR and YCOR and set them
  ;; NOTE: lowest X/YCOR is -0.5, highest (LENGHT-OF-WORLD + 0.5)
  ask boxes [
    set yBox (id mod ceiling (world-width / boxSize))
    set xBox ((id - yBox) / ceiling (world-width / boxSize))
    set xcor (xBox * boxSize) + (boxSize / 2) - 0.5
    set ycor (yBox * boxSize) + (boxSize / 2) - 0.5
  ]
end


to let-die-not-covering-boxes
  ;; 2) let die boxes not covering Indies/Comms with measured attitude
  ;; a) BOXes compute X/YCOR of turtles they cover
  ;;    and assign TURTLES into COVERED-TURTLES
  ask boxes [
    ;; Initial values
    set yBoxMin (ycor - (boxSize / 2) - 0.5) ;; Note: +/-0.5 is here because size of counted turtles is 1,
    set yBoxMax (ycor + (boxSize / 2) + 0.5) ;;       generally it should not be 0.5,
    set xBoxMin (xcor - (boxSize / 2) - 0.5) ;;       but 1/2 of size of counted turtles.
    set xBoxMax (xcor + (boxSize / 2) + 0.5)

    ;; Correcting values - we stop BOXes from counting beyond borders of the wrapped world
    if yBox = 0 [set yBoxMin -0.5] ;; Boxes in down-most row stop counting at Y = -0.5 and do not go lower (which would mean that they follow scannig at the top of the wrapped world)
    if xBox = 0 [set xBoxMin -0.5] ;; Boxes in left-most column stop counting at X = -0.5 and do not go more to left (which would mean that they follow scannig at the right of the wrapped world)
    if yBox = max [yBox] of boxes [
      set yBoxMin (-1 + (yBox * boxSize))
      set yBoxMax (world-height - 0.501)
    ]
    if xBox = max [xBox] of boxes [
      set xBoxMin (-1 + (xBox * boxSize))
      set xBoxMax (world-width - 0.501)
    ]

    ;; Assigning TURTLES into COVERED-TURTLES
    set coveredTurtles (other turtles with [xcor > [xBoxMin] of myself and xcor < [xBoxMax] of myself and ycor > [yBoxMin] of myself and ycor < [yBoxMax] of myself])
  ]

  ;; c) BOXes check ATTITUDEs of COVERED-TURTLES
  ask boxes [if not any? coveredTurtles with [attitude = computedAttitude] [die]]
end


to count-and-plot-numbers
  ;; 3) count and plot numbers
  ;; a: BOX count is counted via BOX-NUMBER reporter
  let bn boxNumber ;; we will use BOX-NUMBER many times, so we store it temporary variable BN
  ;print bn

  ;; b: plotting counts and BOX-SIZE
  if bn >= 1 [
    set-current-plot "Box Counting Plot"
    set-current-plot-pen (word "att" computedAttitude)
    let no-boxes log bn 10
    let scale log (1 / boxSize) 10
    plotxy scale no-boxes
    set xAxisList lput scale xAxisList
    set yAxisList lput no-boxes yAxisList
  ]
end

to-report boxNumber
  report count boxes
end
@#$#@#$#@
GRAPHICS-WINDOW
184
30
652
499
-1
-1
5.75
1
10
1
1
1
0
1
1
1
0
79
0
79
1
1
1
ticks
30.0

BUTTON
6
445
61
478
setUp
  reset-timer\n\nsetup\n\nwrite \"Time to initialize environment by \"\nshow timer
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
60
445
115
478
NIL
go
T
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

PLOT
652
141
981
429
% of new and old opinions
NIL
NIL
0.0
10.0
0.0
10.0
true
true
"" ""
PENS
"orange" 1.0 0 -955883 true "" "plot (count turtles with [attitude = 0]) / Na * 100"
"brown" 1.0 0 -6459832 true "" "plot (count turtles with [attitude = 1]) / Na * 100"
"All Old" 1.0 0 -16777216 true "" "plot (count turtles with [attitude < initialAttitudes]) / Na * 100 "
"yellow" 1.0 0 -1184463 true "" "plot (count turtles with [attitude = 2]) / Na * 100"
"green" 1.0 0 -10899396 true "" "plot (count turtles with [attitude = 3]) / Na * 100"
"All New" 1.0 0 -2674135 true "" "plot (count turtles with [attitude >= initialAttitudes]) / Na * 100 "
"Entropy * 30" 1.0 0 -8630108 true "" "plot (entMean * (100 / granularity))"
"meanEnt * 50" 1.0 0 -5825686 true "" "plot 50 * (mean [ent] of turtles)"

SLIDER
981
147
1153
180
%Indies
%Indies
0
25
10.0
1
1
NIL
HORIZONTAL

SLIDER
1158
507
1330
540
distanceToMyself
distanceToMyself
0.5
2
1.414
0.001
1
NIL
HORIZONTAL

SLIDER
981
51
1153
84
initialAttitudes
initialAttitudes
1
10
2.0
1
1
NIL
HORIZONTAL

SLIDER
981
83
1153
116
allAttitudes
allAttitudes
2
15
4.0
1
1
NIL
HORIZONTAL

SLIDER
981
116
1153
149
probOfIndieChange
probOfIndieChange
0.00
0.02
0.004
0.00001
1
NIL
HORIZONTAL

MONITOR
701
96
751
141
% new
count turtles with [attitude >= initialAttitudes] / count turtles * 100
1
1
11

BUTTON
114
445
178
478
1Step
go\n
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

SLIDER
6
313
178
346
minIndiesSupportiveness
minIndiesSupportiveness
0
100
50.0
1
1
NIL
HORIZONTAL

SLIDER
6
346
178
379
minIndiesPersuasiveness
minIndiesPersuasiveness
0
100
50.0
1
1
NIL
HORIZONTAL

SLIDER
981
180
1153
213
lengthOfWorld
lengthOfWorld
9
79
79.0
1
1
NIL
HORIZONTAL

INPUTBOX
1065
244
1153
304
stopTicks
1460.0
1
0
Number

SLIDER
6
412
178
445
distanceOfListenedIndies
distanceOfListenedIndies
-1
10
0.4
0.05
1
NIL
HORIZONTAL

SLIDER
6
379
178
412
listenedIndiesOnAverage
listenedIndiesOnAverage
0
70
20.0
1
1
NIL
HORIZONTAL

MONITOR
652
96
702
141
all new
aAll
17
1
11

INPUTBOX
981
244
1065
304
RS
2.0
1
0
Number

MONITOR
840
96
903
141
new Indies
aIndies
17
1
11

MONITOR
751
96
841
141
new Commoners
aComms
17
1
11

SLIDER
981
212
1153
245
densityOfAgents
densityOfAgents
0
100
100.0
1
1
NIL
HORIZONTAL

SWITCH
1329
507
1419
540
record?
record?
0
1
-1000

INPUTBOX
981
304
1153
364
filenameForRecordingResults
BlahaG10V01.csv
1
0
String

SWITCH
1158
540
1330
573
preferentialOmitting?
preferentialOmitting?
0
1
-1000

TEXTBOX
722
429
1132
519
PREFERENTIAL-OMITTING? changes way how is world initialized spatially in case the DENSITY is lower than 100%. If the switch is TRUE then we randomly find a few holes in full complete world and then we continue with digging holes, but only next to existing holes. Result is structure with spots completely settled by agents and spots (almost) without agents. If the switch is FALSE, then all holes are digged fully randomly, so the density is homogenously distributed to each part of the world.
11
0.0
1

TEXTBOX
2
0
184
309
INDEPENDENT VARIABLES/INPUTS:\n\nMIN-INDIES-SUPPORTIVENES and MIN-INDIES-PERSUASIVENESS: minimal values of SUPPORTIVENESS and PERSUASIVENESS of INDIES. The higher values the more supportive or persuasive INDIES are.\n\nLISTENED-INDIES-ON-AVERAGE: random-Poisson distributed integer stating to how many INDIES agents listen on average.\n\nDISTANCE-OF-LISTENED-INDIES: listened INDIES influence their listeners regardless the distance, but the distance is crucial listened INDIES' weight (w = 1/d^2). So we set distance of listened INDIES as constant to give them some weight. The lower value the bigger weight.
11
0.0
1

TEXTBOX
186
1
646
30
Small stars are INDIES who changed their opinion because of social pressure (DSIT).\nBig stars are INDIES who changed their opinion independently. Squares are COMMONERS.
11
0.0
1

TEXTBOX
1160
11
1411
511
CONTROL VARIABLES/INPUTS:\n\nDISTANCE-TO-MYSELF: In the equations inside model there is the risk of division by zero - when agent supports herself (SUPPORT and PERSUASION are divided by squared distance, if the distance would be 0 then we would divide by 0). Nowak et al. (1990) suggested to add 1.414 to all distances. We parametrized it - you could use original value or try another values different from 0.\n\nINITIAL-ATTITUDES and ALL-ATTITUDES: number of starting conservative opinions and number of all opinions (conservative + avant-garde).\n\nPROB-OF-INDIES-CHANGE: probability an Indie changes her opinion independently.\n\n%INDIES: percentage of INDIES. \n\nLENGTH-OF-WORLD: size of one side of squarre world - 1 (MAX-PXCOR).\n\nDENSITY-OF-AGENTS: percentage of patches settled by exactly 1 agent.\n\nRS: sets number of random seed.\n\nSTOP-TICKS: length of simulation in ticks. \n\nENT?: Switch for automatic computing of entropy.\n\nRECORD?: switch for recording results of every simulation step to the file FILENAME-FOR-RECORDING.
11
0.0
1

TEXTBOX
655
2
974
92
DEPENDENT VARIABLES/OUTPUTS:\nThese monitors and graph show how many avant-garde and conservative opinions holders are in simulation. Monitors show mainly counts, graph only percentages. Monitors show aggregated avant-garde opinions together - they do not differ between green and yellow opinion.
12
0.0
1

BUTTON
165
499
307
532
compute fractal dimension
count-box-fractal-dimension
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

PLOT
716
516
1129
738
Histogram of individual entropies of NEIs
NIL
NIL
0.0
2.1
0.0
10.0
true
false
"" ""
PENS
"default" 0.1 1 -16777216 true "" "histogram [ent] of turtles"

SLIDER
981
18
1153
51
granularity
granularity
1
10
3.0
1
1
NIL
HORIZONTAL

MONITOR
902
96
981
141
H of Entropy
precision (entMean) 3
17
1
11

SLIDER
124
532
240
565
initialBoxSize
initialBoxSize
0.5
10
1.1
0.1
1
NIL
HORIZONTAL

SLIDER
148
565
240
598
increment
increment
0.1
10
0.1
0.1
1
NIL
HORIZONTAL

SLIDER
6
532
124
565
computedAttitude
computedAttitude
0
14
3.0
1
1
NIL
HORIZONTAL

BUTTON
6
499
77
532
clear boxes
ask boxes [die]
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
77
499
165
532
prepare turtles
ask turtles [\nset size 1\nset shape \"square\"\n]
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

PLOT
307
499
717
738
Box Counting Plot
log (1 / box length)
log (number of boxes)
-1.0
0.0
0.0
4.0
true
true
"" ""
PENS
"trend0" 1.0 0 -16777216 false "" ""
"att0" 1.0 2 -955883 true "" ""
"att1" 1.0 2 -6459832 true "" ""
"att2" 1.0 2 -1184463 true "" ""
"att3" 1.0 2 -10899396 true "" ""
"trend1" 1.0 0 -16777216 false "" ""
"trend2" 1.0 0 -16777216 false "" ""
"trend3" 1.0 0 -16777216 false "" ""

SLIDER
6
565
98
598
repeats
repeats
3
100
30.0
1
1
NIL
HORIZONTAL

MONITOR
98
565
148
610
boxSize
precision boxSize 1
17
1
11

CHOOSER
981
364
1153
409
entropyMeasure
entropyMeasure
"power of opinions" "distance of opinions" "count of opinions" "none"
1

PLOT
1128
573
1330
738
Ln of DIST
NIL
NIL
-2.0
7.0
0.0
10.0
true
false
"clear-plot\nset-plot-x-range (floor ln min [dist] of turtles) (ceiling ln max [dist] of turtles)" ""
PENS
"all" 1.0 1 -16777216 true "" "histogram [ln dist] of turtles"
"indies" 1.0 1 -2674135 true "" "histogram [ln dist] of indies"

SLIDER
46
646
218
679
sampleSize
sampleSize
1
100
25.0
1
1
NIL
HORIZONTAL

SLIDER
46
679
218
712
changeFactor
changeFactor
1
100
100.0
.1
1
NIL
HORIZONTAL

@#$#@#$#@
## Main issue with documentation:

I am afraid that the documentation is "too much speaking", so I really apreciate comments how to write it more concisely.

## WHAT IS IT? (Model scope/ ODD purpose statement)

The model's purpose is to explore the diffusion of avant-garde opinions in a society - the diffusion happens through the means of social pressure: support for the same and persuasion against different opinions, the result is change or standing on own opinion. More specifically, the model seeks to answer how traits of Indies accelerate opinion diffusion. Indies were introduced to sociological theory in 1920’s by Czech classic Bláha. They are special kind of social actors, more independent than others, so they are able to invent novel ideas, better stand the social pressure, present the invented ideas by their way of living, and by doing so change the society from the bottom through the interpersonal interactions. 

The main question addressed is: “Which kind of traits do Indies need for sufficient pace of avant-garde opinions diffusion?” The model explores whether the only creative Indies are able to satisfy sufficient diffusion or whether we also need additional traits of Indies: maximal supportiveness, maximal persuasiveness, and ability to asymmetrically warp social space. 

The model studies the spread of avant-garde opinions at middle size society: from 200 to 6,400 agents (depending on scenario). This size mimics the size of publics surrounding local issues. It is theoretically possible to model larger societies by the slider LENGHTH-OF-WORLD, but with size over 80 x 80 the model start to be very slow - so the larger societies are practically impossible to model with present version of model.

## HOW IT WORKS (Agents, their variables, pseudocode, )

In the model there are two kinds of agents: _INDIES_ (represented by stars) and _COMMONERS_ (represented by squares). Only INDIES introduce avant-garde opinions, while COMMONERS possess no special traits. In some scenarios INDIES could also asymetrically warp the social space - they could be listened as close friend (but not affected) by distant agents (both INDIES and COMMONERS). But both types of agents possibly support agents with the same opinion and persuade agents of different opinions. INDIES are the creative agents, only they are able to introduce avant-garde opinions. COMMONERS are typical agents with no special traits. Diffusion of avant-garde opinions happens through the persuasion.

### Pseudocode:

``` Initialize (SETUP): ```

  * We set random seed to the value of input RS.
  * We ask patches to SPROUT 1 COMMONER (number of sprouting patches is affected by DENSITY slider: DENSITY / 100 * number of patches).
  * We ask COMMONERS to store their neighborhood (r=8.586) into turtleset-variable NEI
  * We set shape (square), PERSUASIVENESS and SUPPORTIVENESS (0-100) of COMMONERS and color them according opinion (0=orange, 1=brown, 2=yellow, 3=green).
  * We turn breed of 10% randomly selected COMMONERS into breed INDIES, percentage is affected by slider %INDIES.
  * We re-set shape (star), PERSUASIVENESS and SUPPORTIVENESS of INDIES according scenario (scenario affects sliders MIN-INDIES-SUPPORTIVENESS and MIN-INDIES-PERSUASIVENESS, values are then on scales from MIN-... to 100; if the sliders MIN-... are set to 100, the values of PERSUASIVENESS and SUPPORTIVENESS are set to 100).
  * We ask all turtles (both INDIES and COMMONERS) to pick out INDIES they will listen to, number of listened INDIES is Poisson-random integer affected by slider LISTENED-INDIES-ON-AVERAGE, and turtles store the selected INDIES into agenset-variable MY-INDIES, INDIES store their listeners into agentset-variable LISTENERS
  * We check whether some of MY-INDIES are stored in turtleset-variable NEI, if they are we pull them out of NEI and leave them only in MY-INDIES 
  * We ask all turtles to set CHANCE-OF-CHANGE? = TRUE, this variable indicates that the turtle's opinion might be changed, because there was a change of opinion in her neighborhood (NEI) or listened INDIES (MY-INDIES).
  * We reset ticks.
  * If the switch RECORD? is TRUE, we prepare for recording the file indicated by input FILENAME-FOR-RECORDING-RESULTS. 



``` Iterative / Tick (GO): ```

  * We ask turtles with CHANCE-OF-CHANGE? = TRUE to: (1) set this indicator FALSE and (2) scan their neighborhood (NEI) and MY-INDIES for support and persuasion 
  _**(this process of scanning will be described in detail bellow).**_ 
  _(Note: turtles without change in their neighborhood (NEI) and MY-INDIES are not asked for scaning - it would be just wasting of time.)_ 
  * If a turtle changes opinion as result of scanning neighborhood (NEI) and listened INDIES (MY-INDIES), she updates ATTITUDE to the value stored in NEW-ATTITUDE (_see procedure `DO-PERSUASION` bellow_), and then she updates: (1) color according ATTITUDE (0=orange, 1=brown, 2=yellow, 3=green), (2) SUPPORTIVENESS and PERSUASIVENESS on interval 0-100 (_Note: also INDIES if they were persuaded_), (3) CHANCE-OF-CHANGE? = TRUE, (4) she asks neighborhood (NEI) to set CHANCE-OF-CHANGE? = TRUE, (5) INDIE asks also her LISTENERS to set CHANCE-OF-CHANGE? = TRUE, and (6) she sets CHANGED? = FALSE 
  * If an INDIE throws on "dice" (random-float 1) number less than 0.00472, then the INDIE independently changes: (1) sets randomly ATTITUDE to one of all four possible opinions, (2) recolors according ATTITUDE (0=orange, 1=brown, 2=yellow, 3=green), (3) re-sets SUPPORTIVENESS and PERSUASIVENESS according scenario (either (a) to 100 or (b) on interval 0-100), (4) sets CHANCE-OF-CHANGE? = TRUE, and (4) asks NEI and  her LISTENERS to set CHANCE-OF-CHANGE? = TRUE.
  * We recount agregated variables on avant-garde opinion spread and record data into file indicated by input FILENAME-FOR-RECORDING-RESULTS.



``` Scanning of neighborhood and listened INDIES (DO-PERSUASION): ```

  * A turtle scans neighborhood (NEI) and listened INDIES (MY-INDIES) whether are there different opinions than her 
  * If no, she stops scanning, because she is surrounded by the same opinion as her
  * If there are different opinions the turtle stores them to temporary list CHALLENGING-ATTITUDES in random order and in this random order she compares PERSUASIVENESS of different opinions with SUPPORTIVENESS for her opinion.
  * Overall PERSUASIVENESS and overall SUPPORTIVENESS are computed as follows:

Persuasion = N<sub>p</sub><sup>1/2</sup> * [Σ(p<sub>i</sub>/d<sub>i</sub><sup>2</sup>)/N<sub>p</sub>]     	(equation 1)
Support    = N<sub>s</sub><sup>1/2</sup> * [Σ(s<sub>i</sub>/d<sub>i</sub><sup>2</sup>)/N<sub>s</sub>]     	(equation 2)
N<sub>p</sub>, N<sub>s</sub>: number of persuading or supporting agents 
p<sub>i</sub>, s<sub>i</sub>: persuasion or support of agent **i** 
d<sub>i</sub>: distance of agent **i** from comparing agent (agent being in the center of social situation).
 
  * If a turtle finds opinion with stronger PERUASION than SUPPORT of her own opinion, then turtle immediately changes to this opinion with strong persuasion (marks herself by indicator CHANGED? = TRUE, and stores the persuading opinion in variable NEW-ATTITUDE) 
_(Note: There are up to three persuading opinions, but if the turtle is persuaded by the first she immediately changes opinion and do not inspect other persuading opinions.)_


### Turtles' variables:

  ATTITUDE: stores opinion of the agent; here we follow theory: attitude is an internal state of an actor, opinion is expressed attitude, that is why we use variable name ATTITUDE and not opinion 

  PERSUASIVENESS: how much is turtle able to persuade holders of different opinions, it is her contribution to overall PERSUASION
  
  SUPPORTIVENESS: how much is turtle able to support holders of same opinion, it is her contribution to overall SUPPORT

  SUSCEPTIBILITY:   number of INDIES the turtle is listening to

  MYINDIES: turtleset-variable, where turtle stores the INDIES which she listens to
    
  NEI: turtleset-variable where are stored the turtles in the neighborhood (r=8.586)

  CHANGED?: indicator whether the turtle was persuaded to change opinion
  
  NEW-ATTITUDE: opinion to which the turtle was persuaded
  
  CHANCE-OF-CHANGE?: indicator of possibility of change - whether something changed in NEI or in MY-INDIES; if all affecting turtles (NEI, MY-INDIES) are in same state as during step before and the turtle was not changed during step before, there is no chance for change (only a change could produce a change, if there is no change we could not expect change of affected turtle) _Note: This variable serves for saving time - to avoid counting of persuasion in situation with no potential for a change._

  LISTENERS: INDIES only variable - INDIES store in this turtleset-variable all turtles who listen to her. It is used only for saving time - in case of INDIE change she let know LISTENERS she has changed and ask them to set CHANCE-OF-CHANGE? = TRUE. LISTENERS do not affect the INDIE they listen to (INDIE might be affected only in case the INDIE is part of LISTENERS' NEI, but then the INDIE is affected because she is a part of NEI, not because she is listened).

### Global variables:

  POSSIBLE-ATTITUDES: list of all four possible attitudes [0 1 2 3]
  
  NI: count of INDIES
  
  NA: count of all turtles

  AINDIES: count of INDIES with avant-garde ATTITUDES [2 3]

  ACOMMS: count of "normal" turtles with avant-garde ATTITUDES [2 3]

  AALL: count of all turtles with avant-garde ATTITUDES [2 3]

### Environment:

Model environment is spatial - squared lattice 80 x 80 (but could be sized down by LENGHT-OF-WORLD slider up to 20 x 20). Each patch of this world sprouts at the start maximally 1 turtle - if the turtles' density given by slider DENSITY is bellow 100, then only respective fraction of randomly chosen patches sprout 1 turtle, if DENSITY = 100 then every patch sprouts 1 turtle. If the switch PREFERENTIAL-OMMITING? = TRUE and DENSITY < 100, then each patch sprouts 1 turtle, then several randomly chosen turtles die (number of chosen turtles = 100 - DENSITY) and then turtles next to the empty patches are randomly killed until there are as much turtles alive as is stated by slider DENSITY. (Note: DENSITY states fraction of turtles alive, but we start killing with count 100 - DENSITY). PREFERENTIAL-OMMITING? = TRUE leads to more realistic spatial structure - there are several spots with fully occupied neighborhood, several empty spots and several spots with very low density. PREFERENTIAL-OMMITING? = FALSE leads to more homogenous structure of empty patches.

Besides the structure there are no special traits of environment.  


## HOW TO USE IT

Setup of model is standard - by button SETUP, model is also standardly run by button GO, which runs forever, resp. 1460 ticks. A step of model we could do by button 1STEP.

By sliders MIN-INDIES-SUPPORTIVENESS and MIN-INDIES-PERSUASIVENESS we could set strength of the INDIES, the closer value to 100 the stronger INDIES. We could also manipulate the number of distant listeners to INDIES by slider LISTENED-INDIES-ON-AVERAGE - the higher value the more INDIES will turtles listen to and the more LISTENERS INDIES will have. If we would like avoid effect of distant listening, set the slider LISTENED-INDIES-ON-AVERAGE to 0 - then no turtle will listen to any INDIE. The weight of listened INDIES is set by slider DISTANCE-OF-LISTENED-INDIES - the shorter distance the bigger weight of INDIES (w = 1/d<sup>2</sup>). It is possible to play by other sliders, inputs etc. see their description bellow. But definitely change the input **RS** - this changes random seed, with unchaged RS you usually receive same result.

Main output is number of avant-garde opinion holders after 1460 ticks (4 simulated years). We could see it at monitor ALL-NEW or as a red line on the graph. In monitor %-NEW we could see percentage of them and on monitors NEW-COMMONERS and NEW-INDIES how the count of avant-garde opinions holders are distributed between INDIES and COMMONERS. In the graph we could also see the fraction of all conservative opinions, and fraction of each opinion: orange, brown, yellow and green.

As successful spread of avant-garde opinions we could take if these opinions together spread into 20% of the simulated public. 20% is twice the fraction of INDIES (set to 10% as default), so it means the INDIES convince enough COMMONERS (11% at least, but practically only half of INDIES have avant-garde opinions, not all of them, so 20% spread means convincing of 16%-17% of COMMONERS). 20% also means established minority.   


### Inputs (sliders, switches, inputs etc. from INTERFACE):
#### Main independent variables / inputs:
MIN-INDIES-SUPPORTIVENESS and MIN-INDIES-PERSUASIVENESS: minimal values of SUPPORTIVENESS and PERSUASIVENESS of INDIES. The higher values the more supportive or persuasive INDIES are. So, by these sliders we could make INDIES stronger (more supportive and persuasive) than COMMONERS. Note: we set minimal threshold, not exact value, in case we set slider MIN-INDIES-SUPPORTIVENESS to 80, the INDIES would have SUPPORTIVENESS in range 80-100.

LISTENED-INDIES-ON-AVERAGE: random-Poisson distributed integer stating to how many INDIES agents listen on average.

DISTANCE-OF-LISTENED-INDIES: listened INDIES influence their listeners regardless the distance, but the distance is crucial listened INDIES' weight (weight = 1/distance<sup>2</sup>). So we set distance of listened INDIES as constant to give them some weight. The lower value the bigger weight.

#### Control variables / inputs:
DISTANCE-TO-MYSELF: In the equations inside model there is the risk of division by zero - when agent supports herself (SUPPORT and PERSUASION are divided by squared distance, if the distance would be 0 then we would divide by 0). Nowak et al. (1990) suggested to add 1.414 to all distances. We parametrized it - you could use original value or try another values different from 0. (See detailed discussion bellow, at the end of ODD documentation.)

INITIAL-ATTITUDES and ALL-ATTITUDES: number of starting conservative opinions and number of all opinions (conservative + avant-garde).

PROB-OF-INDIES-CHANGE: probability an INDIE changes her opinion independently. 

%INDIES: percentage of INDIES.  

LENGTH-OF-WORLD: size of one side of squarre world - 1 (MAX-PXCOR).

DENSITY-OF-AGENTS: percentage of patches settled by exactly 1 agent.

STOP-TICKS: length of simulation in simulated days / ticks. So now after 1460 (= 4 years) the simulation stops.

RS: number of random seed.\n\nSTOP-TICKS: length of simulation in ticks. 

RECORD?: switch for recording results of every simulation step to the file indicated in input FILENAME-FOR-RECORDING.

FILENAME-FOR-RECORDING: indicates name of the file where we record output data.

PREFERENTIAL-OMITTING?: changes way how is world initialized spatially in case the DENSITY is lower than 100%. If the switch is TRUE then we randomly find a few holes in full complete world and then we continue with digging holes, but only next to existing holes. Result is structure with spots completely settled by agents and spots (almost) without agents. If the switch is FALSE, then all holes are digged fully randomly, so the density is homogenously distributed to each part of the world.

## THINGS TO NOTICE
  * Spatial distribution of opinions - opinions usually form spots/isles, it is also possible to see progressive changes, step-by-step expansion or diminishing
  * Change of opinions over the course of time - only rarely it has the shape os S-curve

## EXTENDING THE MODEL

Count of LISTENERS of INDIES is now distributed random-normally, with values close to the average. I prepared a code for inroduction of scale-free distribution of LISTENERS, but it was time consuming (SETUP takes more than 20 seconds). It would be nice to introduce some more interesting network structures of INDIES LISTENERS. 

## NETLOGO FEATURES

I am especially proud for this code:
```
  ask myIndies [
    if member? self [nei] of myself [
      let newNei (other [nei] of myself)
      ask myself [set nei [newNei] of myself]
    ]
  ]
```
It is double use of MYSELF primitive, MYINDIES are asked to check whether they are part of NEI of MYSELF (single use), if yes they store NEI of MYSELF with themselves pulled out as temporary variable NEW-NEI and then MYINDIES ask back MYSELF (double use) to store NEW-NEI in NEI (NEW-NEI -> NEI).


## RELATED MODELS

There are some similarities with these models from NetLogo library:

  * Rebellion - adaptation of Joshua Epstein’s model of civil violence
  * Voting


# ODD documentation

## Name of model: "How do Indies accelerate diffusion of avant-garde opinions?"
/Author anonymised for the reson of peer-review in ABM course on Complexity Explorer/

The model description follows the ODD (overview, design concepts and details) protocol for describing individual- and agent-based models (Grimm et al. 2010). The model is implemented in NETLOGO v. 6.0.4 (Wilensky 1999; freely downloadable from http://ccl.northwestern.edu/netlogo/download.shtml)

## 1. Purpose. 
(What system we are modelling?) The model was designed to explore questions about diffusion speed of avant-garde opinions through a society. Especially, how do different traits of Indies accelerate the diffusion? Indies (represented by stars) are only creative agents – only they are able to invent avant-garde opinions. In some scenarios Indies also asymmetrically warp the social space – their listeners see them as extremely close in social space, but Indies do not even see them. Commoners (represented by squares) are regular/typical agents.
 
(What we are trying to learn about it?) The model explores whether the creativity of Indies alone is able to introduce sufficient pace of avant-garde opinions diffusion or whether we have to introduce other Indies' traits, as well. And also, whether all these traits together or in any combination are able satisfy sufficient pace of change. And lastly, whether this model could produce change from seemingly nothing (i.e. for some amount of steps the diffusion is neglectful, but then suddenly in several tens of steps there is rapid diffusion of avant-garde opinions).

## 2. Entities, state variables, and scales. 
The model includes only agents, but two types of them: Commoners and Indies. Firstly, every agent is characterized by (1) the opinion she has – opinions are discrete nominal values: orange, brown, yellow, and green. Other opinion different from these four is not possible. The yellow and green opinions are avant-garde – only indies could whenever introduce them into simulation. Every agent is also characterized by (2) the persuasiveness – ability to persuade other agents to adopt the opinion, (3) the supportiveness – ability to stand others persuasiveness and ability to support other holders of same opinion to stand persuasion, and (4) the set of Indies the agent is listening to (note: also Indie could listen to other Indies).

Secondly, the Indies are characterized by probability of random opinion change – they could change opinion every step to existing opinion or they could invent presently non-existent opinion (either avant-garde or extinct conservative). The probability that Indie changes opinion at least once during 4 simulated years is 99.9%.

Thirdly, all agents are characterized by their position on the square grid of patches. Every patch might hosts either one Commoner or one Indie - if the DENSITY of agent population is 100% then every patch host an agent, if the DENSITY is lower then respective fraction of patches is empty. The patch distance represents distance in social space – how relevant for the opinion are the opinions of the others. 

Lastly, the units of social space are not specified – distance of two agents is the distance of the patches they are standing on. The size of the world is 20 x 20 or 80 x 80 patches and the world has torus topology (i.e. it is wrapped top-down and also left-right). One step of simulation represents one day, simulation lasts for 1460 steps what is equivalent of 4 years. In every simulation there are 10% of Indies and 90% of Commoners. The number of listened Indies is Poisson-distributed with the mean 0 (means no listened Indies), 10, and 25 (depending on scenario). Warped distance of listened Indies is 0, 1, and 2 (depending on scenario) 

## 3. Process overview and scheduling. 
There are two processes in the model. The first is called dynamic social impact (DSIT) – opinion change is induced by opinion distribution in social neighborhood. The neighborhood consists of listened Indies regardless their distance, and Commoners and Indies in the distance 8.586 patches of the social space. In the neighborhood, every agent compares overall persuasion for every other opinion with overall support for her opinion. If any overall persuasion for other opinions is stronger than overall support for the agent’s opinion, then agent changes opinion to one of opinions with stronger overall persuasion. If the overall support is stronger than any overall persuasion, then agent doesn’t change. The second process is random change of Indies. Every step every Indie has probability 0.472% to randomly change opinion to one of different theoretically possible opinions. It is not mere copying – Indie could invent by this process the opinion yet non-existent or revive the extinct opinion, but she could also change her opinion to different existing opinion. Note, the agents do not move in any process.

These two processes are executed in the order we described them, but changes of opinion are not displayed immediately. Every agent just stores information that she has to change and to which opinion she has to change. All changes are displayed at the end of step that is why the execution order does not influence the results. The second process is completely free of interaction influences. The first process is influenced by social neighborhood, but agents interact with the state of others as it was at the end of the previous step, not with the present state.

Persuasiveness and supportiveness are also changed at the end of step. The agents who just have changed opinion also randomly change the value of persuasiveness and supportiveness. The upper thresholds for all agents are 100. Lower thresholds for Commoners are always 0. The most complicated situation is for Indies. If the change of Indie’s opinion is induced by social neighborhood (the first process) the lower thresholds are 0 – Indies change in same way as Commoners because their change was induced by same process. If the change of Indie’s opinion is random (the second process) the lower thresholds vary among simulations – in some both are 0, in some both are 100, in some the former is 0 and later 100, and in some former is 100 and later 0.

The spread of avant-garde opinions is measured by the number of all agents, Indies, and Commoners who hold avant-garde opinions at the end of step. Present version of model stores these measures every step. By recording these counts of avant-garde opinions holders we could measure: (a) in how many steps reached simulation 10% or 20% of avnt-garde opinion spread, (b) what was the maximum dissemination of avant-garde opinions during 4 simulated years, and (c) how long the spread of avant-garde opinions was over 10% or 20%. So we could measure speed and stability of avant-garde opinion dissemination.

### Pseudo-code:
#### Setup {
	Create 1 COMMONER on every patch
	Turn 10% of COMMONERS into INDIES
	Let agents randomly choose OPINION out of 2 (not all 4) 
	Set agents PERSUASIVENESS and SUPORTIVENESS (randomly in interval 0 - 100) 
	Let agents choose listened INDIES 
	Let agents store the NEIGHBORHOOD [listened INDIES, COMMONERS and INDIES in distance 8.586]
	Record number of agents holding avant-garde opinions (which is 0 in all scenarios now)
}

#### Step [repeat 1460 times] {
	Let every agent compute overall SUPORTIVENESS for her OPINION in her NEIGHBORHOOD
	Let every agent compute overall PERSUASIVENESS for other OPINIONs in her NEIGHBORHOOD 
	Let every agent compare SUPORTIVENESS with every computed PERSUASIVENESS
		If the SUPORTIVENESS is highest [do nothing]
		If at least one of PERSUASIVENESS is higher than SUPORTIVENESS [
			Choose randomly one of the OPINIONs with higher PERSUASIVENESS than the SUPORTIVENESS 
			Mark the agent that she will have to change to chosen OPINION at the end of step]
	Let the INDIES chance to change randomly
		If they succeed [let them randomly choose any theoretically possible different OPINION]
	Display changes of OPINIONs
		If COMMONER changed [set her PERSUASIVENESS and SUPORTIVENESS randomly 0–100]
		If INDIE changed [set her PERSUASIVENESS and SUPORTIVENESS randomly 0 – 100 or 100, according scenario and the reason of change]
	Record number of agents holding avant-garde OPINIONs 
}

## 4. Design concepts.
### Basic principles. 
The basic principle addressed by this model is the spread of avant-garde opinions through the DSIT process, which was coined by Latané (1981). Avant-garde opinions might be whenever introduced only by special kind of agents – Indies. Other kind of agents – Commoners – could just copy these opinions, i.e. change their opinion to one of these avant-garde opinions. Model incorporates only mechanisms for opinions change, regardless their avant-gardeness. There is no possibility to move to another place, or change the Indies agent is listening to.

### Emergence. 
The key result of the model is the number of avant-garde opinions holders after simulated 4 years (1460 steps). We also measure number of avant-garde opinions holders after every step, and immediately after initialization. Adoption of avant-garde opinions is random (Indies, 2nd process) or deterministic (all agents, DSIT: 1st process). Indies introduce avant-garde opinions to the simulation, but then opinion change of all agents is influenced by deterministic DSIT process. So ex definicio, there should be some avant-garde opinions holders, because Indies could change their opinions to the avant-garde ones whenever (and there are 10% of Indies among the agents). The question is, whether there are enough avant-garde opinion holders, at least more than number of Indies, i.e. if there is salient number of avant-garde opinions holders among Commoners.

### Adaptation. 
The agents adapt their opinions according opinions of agents in their neighborhood. This adaptation happens through the DSIT process. During DSIT every agent compares overall support for her opinion with overall persuasions for different opinions in her neighborhood. In case the support is the strongest, nothing happens. In case some persuasions are stronger than support, one of opinions with stronger persuasion is chosen randomly and the agent changes her opinion to this chosen opinion.

### Objectives. 
DSIT is assumed to be a priori objective of agents – agents change to opinions with stronger overall persuasion as a result of the rule, not because it raises their chance to meet other objectives. To carry out DSIT is the only objective of the agents.

### Learning. 
There is no learning in the model.

### Prediction. 
There is no prediction in the model.

### Sensing. 
All agents sense four variables: (1) the opinion, (2) the supportiveness, (3) the persuasiveness, and (4) the distance. They sense them in their neighborhood: (a) themselves, (b) listened Indies, and (c) the Indies and Commoners in the radius 8.586 patches of social space. Agents simply know these variables accurately – there is no special sensing mechanism modeled and no noise in sensed information. Behind their neighborhood agents sense nothing.

### Interaction. 
All agents interact indirectly: they synchronize their opinions through the DSIT process – agents do (not) change their opinions according the distribution of opinions in their neighborhoods. There is no other interaction.

### Stochasticity. 
There are four stochastic processes: (1) random change of Indies’ opinion, (2) random choice between two or three successfully persuading opinions, (3) assigning values of persuasiveness and supportiveness, and (4) opinion composition during initialization. Each of them has different reason. Firstly, original theory introducing Indies does not state any mechanism how indies invent avant-garde values, or revive extinct conservative value, or why they change to different existing opinion. The original theory just states that it happens sometimes (Kalvas & Janák 2016, 2017). The source for this Indies’ behavior is their high creativity, so it would be illogical to model creative process through deterministic concept. We model this every step by probability 0.472% that an Indie changes her opinion to different theoretically possible one at the end of step. This small probability means that every Indie has 99.9% probability to change her opinion at least once during 1460 steps of simulation.

Secondly, in original model of Nowak, Szamrej & Latané (1990) there were only two struggling opinions, we made model general for N opinions. But then problem arisen – how to handle situations when more than one opinion has the overall persuasiveness stronger than the overall supportiveness of the comparing agent. Our argument for random choice is following: in case the agent feels persuaded, she probably immediately change to persuading opinion and doesn’t wait until the end of the day and doesn’t collect persuading information on other opinions. But which of persuading opinions will be the first and successful is more or less random. So, that is why we choose persuading opinion randomly.

Thirdly, we followed original model (ibid.), where authors assigned values of supportiveness and persuasiveness randomly on the scale from 0 to 100. We have no theory or hypothesis regarding spatial composition or correlation of these traits. We only test the original hypothesis of Bláha that Indies have maximal value of supportiveness and our hypothesis that the Indies need also maximal value of persuasiveness for the effective spread of avant-garde values (Kalvas & Janák 2016, 2017). So, in some scenarios we do not assigned random value but value 100. 

Fourthly, the starting composition of opinions is random, as well. We do not test any effects of initial opinions compositions. As shows Nowak et al (1990), from random opinion composition structure quickly emerges – in their model without Indies it takes 30 steps at maximum and DSIT process finds its equilibrium and stops. We are mainly focused on ability of Indies to change this emerged opinion structure by introducing additional change, and especially by introducing avant-garde opinions. 

### Collectives. 
There are no collectives in the model.

### Observation. 
We measure number of avant-garde opinions holders at the end of every step and immediately after initialization. These measures tell us what is pace of spread of avant-garde opinions. Specifically, they help us to identify cases where salient spread of avant-garde opinions happened from seemingly nothing.

## 5. Initialization. 
The size of the world is 20 x 20 or 80 x 80 patches and the world has torus topology (i.e. it is wrapped top-down and also left-right). Every patch sprouts one Commoner, in some cenarios only 70% of patches sprout a Commoner. Then randomly chosen 10% of Commoners are turned into Indies, so there are 90% of Commoners and 10% of Indies. Then every agent chooses an integer number that is Poisson-random distributed with mean 0, 10 or 25. The agents then randomly choose for listening as many Indies as the random integer they have chosen. Then agents stores their neighborhood that consists of (a) themselves, (b) the Indies chosen for listening, and (c) Indies and Commoners in the radius 8.586 patches of social space. This neighborhood doesn’t change – agents do not move and still listen to Indies chosen during initialization.

Every agent chooses her opinion randomly from two possible conservative opinions (orange and brown). There are also avant-garde opinions (green and yellow), but no agent is allowed to choose them at the start - these avant-garde opinions might be brought to simulation only by Indies in the course of simulation. So at the initialization there are only two posibilities, only two conservative opinions - orange and brown - the agent could randomly choose to initialize into. 

Agents are then assigned by values of supportiveness and persuasiveness randomly on the scale from 0 to 100. Commoners’ values are always assigned from the full scale 0–100. In some scenarios supportiveness or persuasiveness are randomly assigned to Indies, in some we set one or both of them to maximum value (100). By doing this we simulate that Indies previously independently changed their mind to conservative opinions and since the start of the simulation they are also able to invent (arguments for) avant-garde opinions.

## 6. Input data. 
The model does not employ any external data.
Note: here is INPUT defined according ODD as external datafile.

## 7. Submodels. 
Firstly, the DSIT submodel defines rules of opinion change. The opinion change is induced by opinion distribution in social neighborhood. The neighborhood consists of listened Indies regardless their distance, and Commoners and Indies in the distance 8.586 patches of the social space. In the neighborhood, every agent compares overall persuasion for every other opinion with overall support for her opinion. If the overall support is stronger than any overall persuasion, then agent doesn’t change. If any overall persuasion for other opinions is stronger than overall support for the agent’s opinion, then agent changes opinion to one of opinions with stronger overall persuasion. Overall support and overall persuasions are computed according following equations:

Persuasion = N<sub>p</sub><sup>1/2</sup> * [Σ(p<sub>i</sub>/d<sub>i</sub><sup>2</sup>)/N<sub>p</sub>]     	(equation 1)
Support    = N<sub>s</sub><sup>1/2</sup> * [Σ(s<sub>i</sub>/d<sub>i</sub><sup>2</sup>)/N<sub>s</sub>]     	(equation 2)
N<sub>p</sub>, N<sub>s</sub>: number of persuading or supporting agents 
p<sub>i</sub>, s<sub>i</sub>: persuasion or support of agent _i_ 
d<sub>i</sub>: distance of agent _i_ from comparing agent (agent being in the center of social situation).

These equations mean that supportiveness of every agent in the neighborhood is divided by the squared distance of this agent. Such weighted are summarized for every supporting agent (i.e. holder of same opinion). Such a sum is divided by the number of supporters in the neighborhood, i.e. mean weighted support is computed. And lastly, the mean is multiplied by squared root of number of supporters. Analogically, overall persuasions are computed. Note, there are four opinions theoretically possible, the only one is supporting – and then there might be three persuading opinions.

Regarding distances there is one non-intuitive rule. Every agent supports herself, but in that case there would be division by 0. Avoiding this, Nowak et al. (1990) stated that distance scale doesn’t start with 0, but with 1. And because we operate in 2D social space the distance of agent to herself is squared root of 2 (1.414 approximately). In the model this distance is taken as slider variable PARITY, so it is possible to set it to another value different from zero if the user of the model has good reason to change the least distance to 1 or 2 or some another, or just want to try the effect of this the least possible distance. PARITY is the lowest possible distance, so we add 1.414 to every distance we could measure between the agent and another agent. That is why the radius of neighborhood is stated to be 8.586, because the agent sees the farthest neighbor in distance 10 (i.e. squared distance is 100). We think there is no point include other agents – dividing persuasiveness and supportiveness (both on the scale 0–100) doesn’t make sense for bigger dividers than 100. It would not produce real difference, but it would slow down simulation process. 

Regarding distances there is one exception – Indies that asymmetrically warp the social space. Listeners of Indies see them very close – according scenario, as themselves (distance: 1.414) or as the closest friends (distance: 2.414) or friends (distance: 3.414). But Indies see their listeners normally, resp. if the listener is in the radius 8.586, Indie sees her normally, if the listener is farther, the Indie doesn’t see her. The Indies might influence many, but are influenced only by their neighborhood – note, a Indie could be listened by other Indie, so the Indies might be part of neighborhoods of other Indies.

Secondly, Indies randomly change. Every step every Indie has probability 0.472% to randomly change opinion to one of different theoretically possible opinions (i.e. 99.9% of at least one change during 1460 simulation’s steps). It is not mere copying – Indie could invent by this process the opinion yet non-existent or revive the extinct opinion, but she could also change her opinion to different existing opinion. Indies are not mere source of randomness – they are the only source of avant-garde values during whole simulation. 


# Bibliography / Credits:
Gabbriellini, S. (2014). Model implementing  Dynamical Social Impact Theory according Nowak et al. (1990) - model was delivered during NetLogo course at Brescia where author participated as student and Simone Gabriellini as main teacher. Brescia, ITA (July 2014).
Online: https://www.slideshare.net/slideshow/embed_code/37036249

Grimm, V., Berger, U., DeAngelis, D.L., Polhill, J.G., Giske, J. & Railsback, S.F. (2010). The ODD protocol: A review and first update. Ecological Modelling, 211, 2760–2768.

Kalvas, F. & Janák, D. Musí být funkcionalismus statický? Modelování federativního funkcionalismu I. A. Bláhy. 6th Sociological Autumn Conference in Olomouc, Olomouc, CZ (October 2016).

Kalvas, F. & Janák, D. Federativní funkcionalismus I. A. Bláhy: Inovace klasického díla a její prověření pomocí multiagentního modelování. Conference of Czech Sociological Society, Prague, CZ (February 2017).

Latane, B. (1981). The psychology of social impact. American psychologist, 36(4), 343-356.

Nowak, A., Szamrej, J., & Latané, B. (1990). From private attitude to public opinion: A dynamic theory of social impact. Psychological Review, 97(3), 362.

Wilensky, U. (1999) NETLOGO. Center for Connected Learning and Computer-Based Modeling, Northwestern University, Evanston, IL.
See http://ccl.northwestern.edu/netlogo.
@#$#@#$#@
default
true
0
Polygon -7500403 true true 150 5 40 250 150 205 260 250

airplane
true
0
Polygon -7500403 true true 150 0 135 15 120 60 120 105 15 165 15 195 120 180 135 240 105 270 120 285 150 270 180 285 210 270 165 240 180 180 285 195 285 165 180 105 180 60 165 15

arrow
true
0
Polygon -7500403 true true 150 0 0 150 105 150 105 293 195 293 195 150 300 150

box
false
0
Polygon -7500403 true true 150 285 285 225 285 75 150 135
Polygon -7500403 true true 150 135 15 75 150 15 285 75
Polygon -7500403 true true 15 75 15 225 150 285 150 135
Line -16777216 false 150 285 150 135
Line -16777216 false 150 135 15 75
Line -16777216 false 150 135 285 75

bug
true
0
Circle -7500403 true true 96 182 108
Circle -7500403 true true 110 127 80
Circle -7500403 true true 110 75 80
Line -7500403 true 150 100 80 30
Line -7500403 true 150 100 220 30

butterfly
true
0
Polygon -7500403 true true 150 165 209 199 225 225 225 255 195 270 165 255 150 240
Polygon -7500403 true true 150 165 89 198 75 225 75 255 105 270 135 255 150 240
Polygon -7500403 true true 139 148 100 105 55 90 25 90 10 105 10 135 25 180 40 195 85 194 139 163
Polygon -7500403 true true 162 150 200 105 245 90 275 90 290 105 290 135 275 180 260 195 215 195 162 165
Polygon -16777216 true false 150 255 135 225 120 150 135 120 150 105 165 120 180 150 165 225
Circle -16777216 true false 135 90 30
Line -16777216 false 150 105 195 60
Line -16777216 false 150 105 105 60

car
false
0
Polygon -7500403 true true 300 180 279 164 261 144 240 135 226 132 213 106 203 84 185 63 159 50 135 50 75 60 0 150 0 165 0 225 300 225 300 180
Circle -16777216 true false 180 180 90
Circle -16777216 true false 30 180 90
Polygon -16777216 true false 162 80 132 78 134 135 209 135 194 105 189 96 180 89
Circle -7500403 true true 47 195 58
Circle -7500403 true true 195 195 58

circle
false
0
Circle -7500403 true true 15 15 270

circle 2
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240

cow
false
0
Polygon -7500403 true true 200 193 197 249 179 249 177 196 166 187 140 189 93 191 78 179 72 211 49 209 48 181 37 149 25 120 25 89 45 72 103 84 179 75 198 76 252 64 272 81 293 103 285 121 255 121 242 118 224 167
Polygon -7500403 true true 73 210 86 251 62 249 48 208
Polygon -7500403 true true 25 114 16 195 9 204 23 213 25 200 39 123

cylinder
false
0
Circle -7500403 true true 0 0 300

dot
false
0
Circle -7500403 true true 90 90 120

face happy
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 255 90 239 62 213 47 191 67 179 90 203 109 218 150 225 192 218 210 203 227 181 251 194 236 217 212 240

face neutral
false
0
Circle -7500403 true true 8 7 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Rectangle -16777216 true false 60 195 240 225

face sad
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 168 90 184 62 210 47 232 67 244 90 220 109 205 150 198 192 205 210 220 227 242 251 229 236 206 212 183

fish
false
0
Polygon -1 true false 44 131 21 87 15 86 0 120 15 150 0 180 13 214 20 212 45 166
Polygon -1 true false 135 195 119 235 95 218 76 210 46 204 60 165
Polygon -1 true false 75 45 83 77 71 103 86 114 166 78 135 60
Polygon -7500403 true true 30 136 151 77 226 81 280 119 292 146 292 160 287 170 270 195 195 210 151 212 30 166
Circle -16777216 true false 215 106 30

flag
false
0
Rectangle -7500403 true true 60 15 75 300
Polygon -7500403 true true 90 150 270 90 90 30
Line -7500403 true 75 135 90 135
Line -7500403 true 75 45 90 45

flower
false
0
Polygon -10899396 true false 135 120 165 165 180 210 180 240 150 300 165 300 195 240 195 195 165 135
Circle -7500403 true true 85 132 38
Circle -7500403 true true 130 147 38
Circle -7500403 true true 192 85 38
Circle -7500403 true true 85 40 38
Circle -7500403 true true 177 40 38
Circle -7500403 true true 177 132 38
Circle -7500403 true true 70 85 38
Circle -7500403 true true 130 25 38
Circle -7500403 true true 96 51 108
Circle -16777216 true false 113 68 74
Polygon -10899396 true false 189 233 219 188 249 173 279 188 234 218
Polygon -10899396 true false 180 255 150 210 105 210 75 240 135 240

house
false
0
Rectangle -7500403 true true 45 120 255 285
Rectangle -16777216 true false 120 210 180 285
Polygon -7500403 true true 15 120 150 15 285 120
Line -16777216 false 30 120 270 120

leaf
false
0
Polygon -7500403 true true 150 210 135 195 120 210 60 210 30 195 60 180 60 165 15 135 30 120 15 105 40 104 45 90 60 90 90 105 105 120 120 120 105 60 120 60 135 30 150 15 165 30 180 60 195 60 180 120 195 120 210 105 240 90 255 90 263 104 285 105 270 120 285 135 240 165 240 180 270 195 240 210 180 210 165 195
Polygon -7500403 true true 135 195 135 240 120 255 105 255 105 285 135 285 165 240 165 195

line
true
0
Line -7500403 true 150 0 150 300

line half
true
0
Line -7500403 true 150 0 150 150

pentagon
false
0
Polygon -7500403 true true 150 15 15 120 60 285 240 285 285 120

person
false
0
Circle -7500403 true true 110 5 80
Polygon -7500403 true true 105 90 120 195 90 285 105 300 135 300 150 225 165 300 195 300 210 285 180 195 195 90
Rectangle -7500403 true true 127 79 172 94
Polygon -7500403 true true 195 90 240 150 225 180 165 105
Polygon -7500403 true true 105 90 60 150 75 180 135 105

plant
false
0
Rectangle -7500403 true true 135 90 165 300
Polygon -7500403 true true 135 255 90 210 45 195 75 255 135 285
Polygon -7500403 true true 165 255 210 210 255 195 225 255 165 285
Polygon -7500403 true true 135 180 90 135 45 120 75 180 135 210
Polygon -7500403 true true 165 180 165 210 225 180 255 120 210 135
Polygon -7500403 true true 135 105 90 60 45 45 75 105 135 135
Polygon -7500403 true true 165 105 165 135 225 105 255 45 210 60
Polygon -7500403 true true 135 90 120 45 150 15 180 45 165 90

sheep
false
15
Circle -1 true true 203 65 88
Circle -1 true true 70 65 162
Circle -1 true true 150 105 120
Polygon -7500403 true false 218 120 240 165 255 165 278 120
Circle -7500403 true false 214 72 67
Rectangle -1 true true 164 223 179 298
Polygon -1 true true 45 285 30 285 30 240 15 195 45 210
Circle -1 true true 3 83 150
Rectangle -1 true true 65 221 80 296
Polygon -1 true true 195 285 210 285 210 240 240 210 195 210
Polygon -7500403 true false 276 85 285 105 302 99 294 83
Polygon -7500403 true false 219 85 210 105 193 99 201 83

square
false
0
Rectangle -7500403 true true 15 15 285 285

square 2
false
0
Rectangle -16777216 true false 75 75 225 225
Rectangle -7500403 true true 105 105 195 195

square-full
false
0
Rectangle -7500403 true true 0 0 300 300

star
true
0
Polygon -7500403 true true 151 1 185 108 298 108 207 175 242 282 151 216 59 282 94 175 3 108 116 108

target
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240
Circle -7500403 true true 60 60 180
Circle -16777216 true false 90 90 120
Circle -7500403 true true 120 120 60

tree
false
0
Circle -7500403 true true 118 3 94
Rectangle -6459832 true false 120 195 180 300
Circle -7500403 true true 65 21 108
Circle -7500403 true true 116 41 127
Circle -7500403 true true 45 90 120
Circle -7500403 true true 104 74 152

triangle
false
0
Polygon -7500403 true true 150 30 15 255 285 255

triangle 2
false
0
Polygon -7500403 true true 150 30 15 255 285 255
Polygon -16777216 true false 151 99 225 223 75 224

truck
false
0
Rectangle -7500403 true true 4 45 195 187
Polygon -7500403 true true 296 193 296 150 259 134 244 104 208 104 207 194
Rectangle -1 true false 195 60 195 105
Polygon -16777216 true false 238 112 252 141 219 141 218 112
Circle -16777216 true false 234 174 42
Rectangle -7500403 true true 181 185 214 194
Circle -16777216 true false 144 174 42
Circle -16777216 true false 24 174 42
Circle -7500403 false true 24 174 42
Circle -7500403 false true 144 174 42
Circle -7500403 false true 234 174 42

turtle
true
0
Polygon -10899396 true false 215 204 240 233 246 254 228 266 215 252 193 210
Polygon -10899396 true false 195 90 225 75 245 75 260 89 269 108 261 124 240 105 225 105 210 105
Polygon -10899396 true false 105 90 75 75 55 75 40 89 31 108 39 124 60 105 75 105 90 105
Polygon -10899396 true false 132 85 134 64 107 51 108 17 150 2 192 18 192 52 169 65 172 87
Polygon -10899396 true false 85 204 60 233 54 254 72 266 85 252 107 210
Polygon -7500403 true true 119 75 179 75 209 101 224 135 220 225 175 261 128 261 81 224 74 135 88 99

wheel
false
0
Circle -7500403 true true 3 3 294
Circle -16777216 true false 30 30 240
Line -7500403 true 150 285 150 15
Line -7500403 true 15 150 285 150
Circle -7500403 true true 120 120 60
Line -7500403 true 216 40 79 269
Line -7500403 true 40 84 269 221
Line -7500403 true 40 216 269 79
Line -7500403 true 84 40 221 269

wolf
false
0
Polygon -16777216 true false 253 133 245 131 245 133
Polygon -7500403 true true 2 194 13 197 30 191 38 193 38 205 20 226 20 257 27 265 38 266 40 260 31 253 31 230 60 206 68 198 75 209 66 228 65 243 82 261 84 268 100 267 103 261 77 239 79 231 100 207 98 196 119 201 143 202 160 195 166 210 172 213 173 238 167 251 160 248 154 265 169 264 178 247 186 240 198 260 200 271 217 271 219 262 207 258 195 230 192 198 210 184 227 164 242 144 259 145 284 151 277 141 293 140 299 134 297 127 273 119 270 105
Polygon -7500403 true true -1 195 14 180 36 166 40 153 53 140 82 131 134 133 159 126 188 115 227 108 236 102 238 98 268 86 269 92 281 87 269 103 269 113

x
false
0
Polygon -7500403 true true 270 75 225 30 30 225 75 270
Polygon -7500403 true true 30 75 75 30 270 225 225 270
@#$#@#$#@
NetLogo 6.2.2
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
<experiments>
  <experiment name="BlahaG07V13a" repetitions="1" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="1480"/>
    <steppedValueSet variable="RS" first="1" step="1" last="10"/>
    <enumeratedValueSet variable="listenedIndiesOnAverage">
      <value value="2"/>
      <value value="4"/>
      <value value="8"/>
      <value value="16"/>
      <value value="32"/>
      <value value="64"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="distanceOfListenedIndies">
      <value value="0"/>
      <value value="1"/>
      <value value="2"/>
      <value value="3"/>
      <value value="4"/>
      <value value="5"/>
      <value value="6"/>
      <value value="7"/>
      <value value="8"/>
      <value value="9"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="record?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="filenameForRecordingResults">
      <value value="&quot;BlahaG07V13.csv&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="distanceToMyself">
      <value value="1.414"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="probOfIndieChange">
      <value value="0.00472"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="allAttitudes">
      <value value="4"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initialAttitudes">
      <value value="2"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="stopTicks">
      <value value="1460"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="preferentialOmitting?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="densityOfAgents">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="%Indies">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="minIndiesPersuasiveness">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="minIndiesSupportiveness">
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="lengthOfWorld">
      <value value="79"/>
    </enumeratedValueSet>
  </experiment>
</experiments>
@#$#@#$#@
@#$#@#$#@
default
0.0
-0.2 0 0.0 1.0
0.0 1 1.0 0.0
0.2 0 0.0 1.0
link direction
true
0
Line -7500403 true 150 150 90 180
Line -7500403 true 150 150 210 180
@#$#@#$#@
0
@#$#@#$#@
