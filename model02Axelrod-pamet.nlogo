turtles-own [
  culture
  entropy
  counted?
]

links-own [
  related
  differences ;; only technical variable, no need to describe in ODD
]

globals [
  sizes ;; list containing ordered SIZES of regions
  theFirst ;; size of the biggest region
  theSecond ;; size of the second biggest region
]


to setup
  ;; clear everything and setup randomness
  ca
  setup-randomness
  setup-world

  ;; every agent sets randomly every position in her CULTURE.
  setup-agents

  ;; state of links reflects how are the neighbors RELATED.
  setup-links

  ;; seting up global variables
  setup-globals

  ;; end of setup
  reset-ticks
end

;;;;;; SETUP routines ;;;;;;
to setup-randomness
  if randomRS? [set RS new-seed]
  random-seed RS
end

to setup-world
  resize-world 0 (worldSize - 1) 0 (worldSize - 1) ;; establishing new size of the world, new coordinates
  set-patch-size (250 / worldSize)
end

to setup-agents
  ask patches [
    sprout 1 [
      set shape "agent"
      set size 1
      set heading 0
      set culture (n-values události [random ways])
      set entropy find-entropy (culture)
      set counted? false
    ]
    ;; We need also all patches to be white, so let's do it here
    set pcolor white
  ]
end

to setup-links
  ;; create links between pairs of agent on NEIGHBORS4
  ask turtles[
    create-links-with turtles-on neighbors4 [
      set color black
      set thickness 0.75
      set differences []
    ]
  ]

  ;; specify RELATED variable according pair similarity
  ask links [update-related]

  ;; set color of links according RELATED
  ask links [recolor-link]
end

to recolor-link
  set color (related / 100 * 9.9)
end

to update-related
  ;; find which turtles are connected via the link and store their CULTURES
  let cult1 ([culture] of end1)
  let cult2 ([culture] of end2)

  ;; compare the CULTURES, store number of MATCHES and store positions of DIFFERENCES
  ;;;; Preparation of needed variables
  let id 0
  let matches 0
  set differences []

  ;;;; cycle for doing everything we need here
  while [id < události] [
    ;; derive values from both CULTURES
    let x1 (item id cult1)
    let x2 (item id cult2)

    ;; compare values, update MATCHES or! store where CULTURES differ
    ifelse x1 = x2 [set matches (matches + 1)] [set differences (lput id differences)]

    ;; update counter
    set id (id + 1)
  ]

  ;; recount and set variable RELATED
  set related round (matches / události * 100)
end

to setup-globals
  set sizes [] ;; list containing ordered SIZES of regions
  set theFirst 0 ;; size of the biggest region
  set theSecond 0 ;; size of the second biggest region
end
;;;;;; End of SETUP routines ;;;;;;

to go
  ask turtles [
    ;; agent firstly selects one of his neighbors as communication partner.
    let partner (one-of link-neighbors) ;; equivalent here to: (one-of turtles-on neighbors4)
    let ourLink (link-with partner) ;; selection of shared link

    ;; agent tries to communicate with her partner
    ;; - it means she checks whether they are RELATED enough
    ;; to be able communicate together.
    let p ([related] of ourLink)
    if (p > random 100) and (p < 100) [
      ;; In case of successful communication one of comunicating agents
      ;; will change one of the differring behaviors to the way of the partner.
      change-behavior (partner) (ourLink)
    ]
  ]

  ;; consistency and random change
  ask turtles [
    consistency ;; turtle prefer to do all BEHAVIORS in same way
    random-change ;; turtle could randomly change way of doing behavior
  ]

  ;; Kvuli uspore casu to budeme pocitat jednou za N kol (frekvenceMereniEntropie)
  if floor (ticks / frekvenceMereniEntropie) = (ticks / frekvenceMereniEntropie) [ask turtles [set entropy find-entropy (culture)]]


  ;; end of step
  tick
  if (count links with [related > 0 and related < 100] = 0) [
    ask turtles [set entropy find-entropy (culture)]
    count-regions
    record-results
    stop
  ]
end

;;;;; GO routines ;;;;;
to consistency
  ;; the turtle checks the CONSISTENCYPROBABILITY
  if random 100 < consistencyProbability [
    ;; if she succeeds randomly chooses value of one of CULTURE behaviors
    let value (one-of culture)

    ;; and copy it on random position in the CULTURE (it might happen that she copies it to same place - so what)
    set culture (replace-item (random události) culture value)

    ;; Also possible on one line:
    ;; set culture (replace-item (random behaviors) culture (one-of culture)) ;; But seems too much culture :)

    ;; update LINKS of the turtle
    ask my-links [update-related recolor-link]
  ]
end

to random-change
  ;; the turtle checks the RANDOMCHANGE
  if random-float 100 < randomChange [
    ;; if she succeeds randomly changes value of one of randomly chosen CULTURE behaviors
    ;; (it might happen that she changes it to same value/way - so what)
    set culture (replace-item (random události) culture (random ways))

    ;; update LINKS of the turtle
    ask my-links [update-related recolor-link]
  ]
end

to change-behavior [partner ourLink]
  ;; store DIFFERENCES of the LINK
  ;; randomly choose one of differing positions
  let id ifelse-value (sjednotitPaměťNáhodně?) [random události][one-of [differences] of ourLink]

  ;; store values of both partners at CULTURE on randomly chosen position
  let x1 (item id culture)
  let x2 (item id [culture] of partner)

  ;; randomly choose one of them
  let value ifelse-value (1 = random 1.1) [x1][x2]

  ;; we ask both partners to store the value on the position at CULTURE
  set culture (replace-item id culture value)
  ask partner [
    set culture (replace-item id culture value)
    ask my-links [update-related recolor-link]
  ]
  ask my-links [update-related recolor-link]
end

to count-regions
  ;; initialization of temporary variables and killing of black LINKS
  let id 2.5 ;; ID and color number of region
  let changed no-turtles ;; agentset containing agents asked to change
  let changing no-turtles ;; agentset containing agents asking CHANGED to change
  ask links with [related = 0] [die] ;; killing blocking LINKS - no need to distinguish between LINKS
  ask patches [set pcolor black] ;; to contrast killed links

  ;; we randomly choose one white agent and turn her into a different COLOR
  ;; if there are some white agents, choose one randomly and repeat counting size
  while [(count turtles with [not counted?]) > 0] [
    set changing (turtle-set one-of turtles with [not counted?])

    ;; then the agent ask white LINK-NEIGHBORS connected via white link to turn into the COLOR
    while [count changing > 0] [
      ;; we turn the CHANGING to CHANGED and clear CHANGING to be able store here turtles for future change
      set changed changing
      set changing no-turtles

      ask changed [
        set counted? true
        set pen-size id
        set changing (turtle-set changing link-neighbors with [not counted?])
      ]
    ]

    ;; if there are no new recolored agents, the region is complete:
    ;; count the size of region, store in the list and updates ID by 1
    let theSize (count turtles with [pen-size = id])
    set sizes (lput theSize sizes)
    set id (id + 5)
  ]
  ;; if there are no white agents, report number of regions and sizes of the first and the second biggest
  set sizes (sort-by > sizes)
  set theFirst (item 0 sizes)
  if length sizes > 1 [set theSecond (item 1 sizes)]
end

to record-results
  ;; Open already existing file or create the file with header for recording results
  ifelse file-exists? fileName [file-open fileName][
    file-open filename
    file-print "wsize, behvrs, ways, constncy, rchange, rs, regns, first, second, time"
  ]

  ;; Record results
  file-print (word
    (worldSize ^ 2) ", "
    události ", "
    ways ", "
    consistencyProbability ", "
    randomChange ", "
    RS ", "
    length sizes ", "
    theFirst ", "
    theSecond ", "
    ticks
    )
  ;; Close the File
  file-close
end

to-report find-entropy [pam]
  ;; Musime nejprve zjistit, ktere RAMCE jsou v pameti jedince pritomne a ulozit si je do listu
  let hodnoty (remove-duplicates pam)

  ;; Musime inicializovat docasne promenne pro pocitadlo cyklu a sumu entropie
  let id 0
  let entSum 0
  let dp length pam

  ;; Pustime cyklus a spocitame prispevek kazde hodnoty k entropii pameti jedince
  while [id < length hodnoty] [
    ;; musim zjistit kolikrat se hodnota vyskutuje v PAM:
    ;;   - tedy musim udelat docasny list, odkud odstranim aktualne pocitanou hodnotu
    let pomHod (remove (item id hodnoty) pam) ;; tj. kouknu se do listu HODNOTY na pozici ID, a tuhle hodnotu odstrannim z PAM

    ;;   - porovnam delky KULT a docasneho listu
    let pocet (dp - length pomHod)

    ;; spoctu vzorec entropie na zaklade podilu hodnoty v PAM
    ;show (word "id:" id " entSum:" entSum " pocet:" pocet " dp:" dp)
    set entSum entSum + ((pocet / dp) * (log (pocet / dp) ways))

    ;; MUSIM AKTUALIZOVAT ID!!!
    set id (id + 1)
  ]

  ;; Odesleme celkovou hodnotu entropie
  set color (9.9 + (9.9 * entSum))
  report -1 * entSum
end
;;;;; End of GO routines ;;;;;
@#$#@#$#@
GRAPHICS-WINDOW
210
5
727
523
-1
-1
13.0
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
9
0
9
1
1
1
ticks
30.0

BUTTON
8
5
72
40
inicializace
setup
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
70
5
134
40
simulace
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

BUTTON
133
5
205
40
1 krok
go
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
8
39
204
72
události
události
2
30
10.0
1
1
NIL
HORIZONTAL

SLIDER
8
72
204
105
ways
ways
2
25
2.0
1
1
NIL
HORIZONTAL

MONITOR
765
10
994
55
NIL
sizes
17
1
11

MONITOR
765
52
865
97
number of regions
length sizes
17
1
11

INPUTBOX
765
98
994
158
RS
8.56002558E8
1
0
Number

SWITCH
765
157
900
190
randomRS?
randomRS?
1
1
-1000

MONITOR
863
53
921
98
NIL
theFirst
17
1
11

MONITOR
920
53
993
98
NIL
theSecond
17
1
11

SLIDER
8
105
204
138
worldSize
worldSize
5
1000
10.0
5
1
NIL
HORIZONTAL

INPUTBOX
765
192
993
255
fileName
records6.csv
1
0
String

BUTTON
898
158
993
192
DELETE FILE
file-delete fileName
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
8
138
204
171
consistencyProbability
consistencyProbability
0
100
100.0
1
1
%
HORIZONTAL

SLIDER
8
171
204
204
randomChange
randomChange
0
5
0.0
0.01
1
%
HORIZONTAL

SWITCH
8
204
204
237
sjednotitPaměťNáhodně?
sjednotitPaměťNáhodně?
0
1
-1000

PLOT
994
10
1194
152
Entropy vs. time
NIL
NIL
0.0
10.0
0.0
1.0
true
false
"" ""
PENS
"default" 1.0 0 -16777216 true "" "plot mean [entropy] of turtles"

SLIDER
8
237
204
270
frekvenceMereniEntropie
frekvenceMereniEntropie
1
1000
1.0
1
1
NIL
HORIZONTAL

PLOT
994
152
1194
272
Histogram of entropy
NIL
NIL
0.0
1.05
0.0
10.0
true
false
"" ""
PENS
"default" 0.01 1 -16777216 true "" "histogram [entropy] of turtles"

@#$#@#$#@
## Purpose 

Tento model studuje možnost aplikace Axelrodova modelu kultury na problém sociální paměti a na studium podmínek, za kterých v simulované společnosti přetrvá víc než jeden typ paměti/ interpretační rámec. Robert Axelrod (1997) ukázal, že lze kulturu modelovat jako vektor hodnot, přičemž jednotlivé pozice vektoru jsou rysy kultury a hodnoty na těchto pozicích kódují způsoby, jimiž lze dané rysy kultury naplnit. Simulovaní jedinci se pokoušejí komunikovat s náhodně vybranými sousedy, přičemž pravděpodobnost komunikace je dána mírou podobnosti jejich kultur (jak velký podíl pozic z vektoru kultury mají obsazený stejnými hodnotami). Pokud ke komunikaci dojde, zvolí komunikující pár náhodně jednu pozici a sjednotí se na jedné hodnotě, kterou jeden z páru od druhého přijme. Z chaosu počáteční náhodné inicializace se tak rodí regiony jedinců se stejnou kulturou. Pro náš model je podstatný Axelrodův poznatek, že pokud mají kultury více rysů (délka vektoru), než je počet způsobů, jimiž lze rysy naplnit (rozsah hodnot), vždy v celé společnosti nakonec převládne jedna jediná kultura, kterou přijmou za svou všichni simulovaní jedinci. 

Axelrodův model později doplnili Scott Page a Jenna Bednar (???), a to tendencí ke konzistenci - jedinci tíhnou k tomu, aby všechny pozice kulturního vektroru měli obsazené stejnou hodnotou, např. pokud je kultura matrilineární, bude zároveň velmi pravděpodobně i matrilokální a velmi pravdě podobně nebude polygynní. Zkrátka, všímají si faktu, že pokud jeden rys kultury řešíme nějakým způsobem, máme tendenci řešit i další rysy kultury tímto způsobem. Pro náš model je důležité, že došli k závěru, že pokud zavedeme do modelu kultury tendenci ke konzistenci, může se vytvořit v modelu několik nézávislých a odlišných kulturních regionů, navzdory tomu, že kultury mají více rysů (délka vektoru), než je počet způsobů, jimiž lze rysy naplnit (rozsah hodnot).

Axelrodův model totiž aplikujeme na sociální paměť. Zde jsou ekvivalentem kulturních rysů klíčové události a ekvivalentem způsobů jsou interpretační rámce, které na události aplikujeme. Konzistence navržená Pagem a Bednarovou je pro model paměti klíčová - pro sociální paměť je právě konstitutivní, že jeden interpretační rámec aplikujeme na většinu událostí (ideálně na všechny). Sociální paměť definuje právě to, že události interpretujeme ze stejné perspektivy - Dušan Lužný s kolegy (2019) tak rozeznávají např. tři typy české náboženské paměti, tedy že klíčové historické události jsou interpretované z katolické, protestantské a ne-náboženské perspektivy. Krom konceptuální blízkosti, má tendence ke konzistenci další význam - víme, že se ve společnostech udržuje paralelně několik interpretačních rámců a existuje i několik paralelních pamětí, dále víme, že vždy existuje více klíčových událostí než interpretačních rámců, je tedy zřejmé, že bez tendence ke konzistenci nemůže být Axelrodův model kultury validním modelem také pro sociální paměť. Otázkou zůstává jak velký nepoměr mezi počtem událostí a počtem interpretačních rámců model "unese" a bude v něm stále přítomno několik interpretačních rámců, nebo dokonce několik regionů jednotně užívajících jeden, v různých regionech různý interpretační rámec.     

## Entities, variables and scales

There are two types of entities - communicating agents and their links (symbolyzing their relationship). Every agent has CULTURE, which is set of numbers, every position in this set represents one behavior and the number on this position represents the way, how the behavior is fulfilled. Links posses variable RELATED which symbolyzes how similar are the cultures of connected agents. There are also two scales, one setting number of behaviors in the culture, and the second setting the number of ways how every behavior could be fulfilled.  

## Process overview and scheduling

Every step every agent firstly selects one of his neighbors as communication partner. Then she tries to communicate with her partner - it means she checks whether they are RELATED enough to be able communicate together. In case of successful communication one of comunicating agents - the agent or her partner - will change one of the differring behaviors to the way of the partner.
The model is initialized randomly - every agent sets randomly every position in her CULTURE. State of links reflects how are the neighbors RELATED.

## THINGS TO NOTICE

(suggested things for the user to notice while running the model)

## THINGS TO TRY

(suggested things for the user to try to do (move sliders, switches, etc.) with the model)

## EXTENDING THE MODEL

(suggested things to add or change in the Code tab to make the model more complicated, detailed, accurate, etc.)

## NETLOGO FEATURES

(interesting or unusual features of NetLogo that the model uses, particularly in the Code tab; or where workarounds were needed for missing features)

## RELATED MODELS

(models in the NetLogo Models Library and elsewhere which are of related interest)

## CREDITS AND REFERENCES

(a reference to the model's URL on the web if it has one, as well as any other necessary credits, citations, and links)
@#$#@#$#@
default
true
0
Polygon -7500403 true true 150 5 40 250 150 205 260 250

agent
false
0
Polygon -7500403 true true 300 0 225 30 75 30 0 0 30 75 30 225 0 300 75 270 225 270 300 300 270 225 270 75 300 0

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
Circle -7500403 true true 0 0 300

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
Rectangle -7500403 true true 30 30 270 270

square 2
false
0
Rectangle -7500403 true true 30 30 270 270
Rectangle -16777216 true false 60 60 240 240

star
false
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
  <experiment name="experimentG01V01" repetitions="10" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="10000"/>
    <metric>ticks</metric>
    <enumeratedValueSet variable="fileName">
      <value value="&quot;records2.csv&quot;"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="consistencyProbability">
      <value value="0"/>
      <value value="50"/>
      <value value="100"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="worldSize">
      <value value="5"/>
      <value value="10"/>
    </enumeratedValueSet>
    <steppedValueSet variable="behaviors" first="2" step="1" last="10"/>
    <enumeratedValueSet variable="ways">
      <value value="2"/>
      <value value="5"/>
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="randomChange">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="randomRS?">
      <value value="true"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="experimentG01V02" repetitions="50" sequentialRunOrder="false" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="10000"/>
    <metric>ticks</metric>
    <enumeratedValueSet variable="fileName">
      <value value="&quot;records2.csv&quot;"/>
    </enumeratedValueSet>
    <steppedValueSet variable="consistencyProbability" first="0" step="20" last="100"/>
    <enumeratedValueSet variable="worldSize">
      <value value="5"/>
      <value value="10"/>
    </enumeratedValueSet>
    <steppedValueSet variable="behaviors" first="2" step="1" last="10"/>
    <steppedValueSet variable="ways" first="2" step="1" last="10"/>
    <enumeratedValueSet variable="randomChange">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="randomRS?">
      <value value="true"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="experimentG01V03" repetitions="30" sequentialRunOrder="false" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="10000"/>
    <metric>ticks</metric>
    <enumeratedValueSet variable="fileName">
      <value value="&quot;records3.csv&quot;"/>
    </enumeratedValueSet>
    <steppedValueSet variable="consistencyProbability" first="0" step="50" last="100"/>
    <enumeratedValueSet variable="worldSize">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="behaviors">
      <value value="2"/>
      <value value="5"/>
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="ways">
      <value value="2"/>
      <value value="5"/>
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="randomChange">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="randomRS?">
      <value value="true"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="experimentG01V04" repetitions="100" sequentialRunOrder="false" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="10000"/>
    <metric>ticks</metric>
    <enumeratedValueSet variable="fileName">
      <value value="&quot;records4.csv&quot;"/>
    </enumeratedValueSet>
    <steppedValueSet variable="consistencyProbability" first="0" step="50" last="100"/>
    <enumeratedValueSet variable="worldSize">
      <value value="5"/>
      <value value="10"/>
      <value value="15"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="behaviors">
      <value value="2"/>
      <value value="5"/>
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="ways">
      <value value="2"/>
      <value value="5"/>
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="randomChange">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="randomRS?">
      <value value="true"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="experimentG01V05" repetitions="1" sequentialRunOrder="false" runMetricsEveryStep="false">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="10000"/>
    <metric>ticks</metric>
    <enumeratedValueSet variable="fileName">
      <value value="&quot;records5.csv&quot;"/>
    </enumeratedValueSet>
    <steppedValueSet variable="consistencyProbability" first="0" step="50" last="100"/>
    <enumeratedValueSet variable="behaviors">
      <value value="2"/>
      <value value="5"/>
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="ways">
      <value value="2"/>
      <value value="5"/>
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="randomChange">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="randomRS?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="worldSize">
      <value value="5"/>
      <value value="10"/>
      <value value="15"/>
    </enumeratedValueSet>
    <steppedValueSet variable="RS" first="1" step="1" last="50"/>
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

block
0.0
-0.2 0 0.0 1.0
0.0 1 1.0 0.0
0.2 0 0.0 1.0
link direction
true
0
Polygon -7500403 true true 0 150 45 45 150 0 255 45 300 150 255 255 150 300 45 255
@#$#@#$#@
0
@#$#@#$#@
