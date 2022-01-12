extensions [
  gis
  matrix ]
globals
[divisions fractal-dim
  len walk-count old-box-count
  new-box-count x-axis-list y-axis-list
  box-size explore? iteration
  iterations-to-go slope lin-reg-eq
  any-new-boxes?
  r-square
  automatic-bcd?
]
breed         ;used only for taking box counting dimension of fractal examples.
[boxes box]
boxes-own
[past? present? future?]
patches-own [fractal?]

;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;import image;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;

to setup
  clear-all
  if image = "coastline"
  [
    import-pcolors  "Coastline.png"
    ask patches with [pcolor != white]
    [set pcolor black]
  ]
  if image = "tree"
   [
    import-pcolors  "Tree.png"

    ask patches with [pcolor != white]
    [set pcolor black]
  ]

  if image = "custom-image"
  [
    import-pcolors  imageName   ;make sure your image is a square with colors that represent what you're interested in dimensioning (black and white is easiest)
  ]

  set explore? false
  set automatic-bcd? true
  set x-axis-list [ ]
  set y-axis-list [ ]
  ask turtles [die]
  set box-size initial-box-length
  set iteration 0

  reset-ticks
end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;Box-Counting-Dim;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


to bcd-go
  set len initial-box-length
  bcd-begin
end

to bcd-begin
  ask boxes ; clears screen in case there are any old boxes
    [die]

  if automatic-bcd?
    [
      if box-size >= initial-box-length
      [ set automatic-bcd? false]
    ]

  if ticks != 0
    [set box-size box-size + increment
      set iteration iteration + 1
      set iterations-to-go 91 - iteration
    ]
  set old-box-count 0
  set new-box-count 1    ;eliminates an error for first round
  make-initial-box
  make-neighbors-count-patches

end

to setup-bcd
  set box-size initial-box-length
  make-initial-box
end

;makes a starter box at the beginning of each run with present? = true.
;This box will then be used to make boxes with future? = true
;which will be used for the next run.
to make-initial-box
  create-boxes 1
  ask boxes [
    set shape "square"
    set size box-size
    setxy  -60 0
    set heading 90
    set color red
    set past? false
    set present? true
    set future? false
  ]
end

;makes a Moore neighborhood around the starter box and counts patches below each new box (exploit).
;If there are no new boxes with patches under them for a given run a box is sent outside the neighborhhod
;to cover non-contiguous patches (explore). If this box finds no new patches the run is complete.

to make-neighbors-count-patches
  ask boxes with [present? = true ]
    [make-neighbors]

  ask boxes with [future?  = true]
    [exploit]
  count-new-boxes
  if any-new-boxes?     = false
    [explore]

  if any-new-boxes? = false
    [calculate-box-counting-dimension]
  if any-new-boxes? = false and automatic-bcd? [
    bcd-begin
    stop ]
  update-past-present-future-states
  tick
  if any-new-boxes? = true
    [make-neighbors-count-patches]
end

to make-neighbors
  hatch 1 [fd box-size rt 90
    set present? false set future? true
    hatch 1 [fd box-size rt 90
      set present? false set future? true
      hatch 1 [fd box-size
        set present? false set future? true
        hatch 1 [fd box-size rt 90
          set present? false set future? true
          hatch 1 [fd box-size
            set present? false set future? true
            hatch 1 [fd box-size rt 90
              set present? false set future? true
              hatch 1 [fd box-size
                set present? false set future? true
                hatch 1 [fd box-size
                  set present? false set future? true
                ]]]]]]]]
end

to exploit
  if count boxes in-radius (box-size / 2) > 1  ; eliminates duplicates
    [die]

  if count patches-under-me = 0
    [ die ]
end

to-report patches-under-me
  report  patches in-radius  ( (1.4 * size ) / 2 )  with [pcolor = color-value]
  ;   [let my-n-edge ycor + (0.5 * size)  ;; if my shape is a square
  ;   let my-s-edge ycor - (0.5 * size)
  ;   let my-w-edge xcor - (0.5 * size)
  ;   let my-e-edge xcor + (0.5 * size)
  ;   report patches with [pcolor = green and
  ;   (pycor - 0.5 < my-n-edge) and (pycor + 0.5 > my-s-edge) and
  ;   (pxcor + 0.5 > my-w-edge) and (pxcor - 0.5 < my-e-edge)
  ;   ]
  ;  ]
end

to explore
  if count boxes with [present? = true] > 0 [
    ask patches with [pcolor = color-value] [
      ifelse count boxes in-radius  (  box-size ) = 0
      [set explore? true]
      [stop]
    ]
  ]

  if explore? [
    ask one-of boxes with [present? = true] [
      hatch 1 [
        set present? false set future? true
        move-to min-one-of patches with [pcolor = color-value and count boxes in-radius  ( box-size ) = 0 ]
        [distance myself]]
    ]
  ]
  count-new-boxes
  set explore? false
end

to count-new-boxes
  set old-box-count new-box-count
  set new-box-count count boxes
  ifelse old-box-count = new-box-count
  [set any-new-boxes? false]
  [set any-new-boxes?  true]
end

to update-past-present-future-states
  ask boxes [
    if present? = true
    [set past? true set present? false]
    if future?   = true
    [set future? false set present? true]
  ]
end



to calculate-box-counting-dimension

  if count boxes >= 1 [     ; eliminates potential error message if setup is pressed during a box-counting procedure
    set-current-plot "Box Counting Plot"
    set-current-plot-pen "default"
    let no-boxes log (count boxes ) 10
    let scale (log ( 1 / box-size ) 10 )
    plotxy scale no-boxes
    set x-axis-list lput scale x-axis-list
    set y-axis-list lput no-boxes y-axis-list
  ]

  stop
end

to fractal-dimension
  if ticks = 0 [
    if divisions > 0 [ ; eliminates potential error message
      let line-segments count turtles
      set fractal-dim precision(( log  line-segments  10 / log  divisions 10 ))3
      show line-segments
      show divisions
      show fractal-dim
    ]
  ]
  stop
end


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;Linear Reg;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;Linear regression is used to find the 'best-fit' line
;through all the tuples (box-size,number of boxes) plotted in the "Number of Boxes vs. Scale" plot.
;The slope of the line is the box counting dimension.

to linear-regression

  if count boxes >= 1 [  ; eliminates potential error message if setup is pressed during a box-counting procedure [

    let regression matrix:regress matrix:from-column-list (list y-axis-list  x-axis-list)   ;using the regression tool from the matrix extension
                                                                                            ;setting y-intercept and slope (measure of goodness of fit)
    let y-intercept item 0 (item 0 regression)
    set slope item 1 (item 0 regression)
    set r-square item 0 (item 1 regression)


    ; set the equation to the appropriate string
    set lin-reg-eq (word (precision slope 3) " * x + " (precision y-intercept 3))


    set-current-plot "Box Counting Plot"
    set-current-plot-pen "pen-4"
    plot-pen-reset
    auto-plot-off
    plotxy plot-x-min (plot-x-min * slope + y-intercept)
    plot-pen-down
    plotxy plot-x-max (plot-x-max * slope + y-intercept)
    plot-pen-up

  ]
end
@#$#@#$#@
GRAPHICS-WINDOW
259
10
776
528
-1
-1
1.016
1
10
1
1
1
0
0
0
1
-250
250
-250
250
1
1
1
ticks
30.0

BUTTON
13
265
215
301
Image Setup
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

PLOT
780
55
1047
310
Box Counting Plot
log [1 / box length]
log [number of boxes]
-2.0
-1.0
0.0
2.0
true
false
"" ""
PENS
"default" 1.0 2 -16777216 true "" ""
"pen-4" 1.0 0 -2674135 true "" ""

BUTTON
125
348
251
381
Box Counting: Go
BCD-go
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
779
353
1046
387
Linear Regression
linear-regression
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

MONITOR
783
308
904
353
Box-Counting Dim.
slope
7
1
11

MONITOR
86
477
169
522
Box Length
box-size
17
1
11

MONITOR
6
477
88
522
Iteration
iteration
17
1
11

MONITOR
901
309
1013
354
Equation of Line
lin-reg-eq
17
1
11

TEXTBOX
782
396
1052
429
NIL
11
0.0
1

SLIDER
8
153
207
186
color-value
color-value
0
139
35.0
1
1
NIL
HORIZONTAL

TEXTBOX
9
191
260
268
Only one color value in the image can be selected for box counting.  E.g., if color-value is 0, only black patches are included in box counting.   Use \"Color Swatches\" under the \"Tools\" menu to see color values.  
11
0.0
1

TEXTBOX
11
10
308
32
Applied Box-Counting
18
95.0
1

TEXTBOX
10
324
213
346
Box Counting Controls
16
0.0
1

CHOOSER
9
104
208
149
image
image
"coastline" "tree" "custom-image"
2

MONITOR
166
477
248
522
# of boxes
count boxes with [color = red]
17
1
11

MONITOR
997
309
1047
354
R^2
r-square
5
1
11

TEXTBOX
11
48
161
68
Image Examples
16
0.0
1

BUTTON
7
348
126
381
Box Counting: Setup
setup-bcd
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
411
251
444
increment
increment
0
2
1.0
0.1
1
NIL
HORIZONTAL

SLIDER
6
380
251
413
initial-box-length
initial-box-length
1
30
5.0
1
1
NIL
HORIZONTAL

TEXTBOX
784
20
980
42
Box-Counting Plot
16
0.0
1

TEXTBOX
12
447
223
489
Amount that box length increases per iteration of box counting
11
0.0
1

TEXTBOX
11
73
229
115
See Info tab for instructions on how to input your own image. 
11
0.0
1

INPUTBOX
778
414
1013
474
imageName
custom-image4.png
1
0
String

@#$#@#$#@
## WHAT IS IT?
Applied Box-Counting is a model that illustrates how the box-counting method can be applied to any two-dimensional, one-color image.  An image can be imported into NetLogo as a .png file.  

## HOW IT WORKS

See BoxCountingDimension.nlogo in the Fractals series for information on the box-counting method. 

## HOW TO USE IT
In the upper left hand corner of the interface, select an image example, and press "Image Setup."  For example, if you select "coastline", an image of the coast of Britain should appear in the view. 

Only one color value in the image can be selected for box counting.  E.g., if color-value is 0, only black patches are included in box counting.   Use "Color Swatches" under the "Tools" menu to see color values.  

In the coastline example, since the coastline is black in this image, the color value should be black (color-value = 0). Press "Box Counting: Setup" and then "Box Counting: Go". A box-counting run will begin. For each iteration, boxes cover the chosen color, and the system plots the log of the number of boxes, N, versus the log of the scale, 1/r, in the plot one the right side of the interface.  The model will continue iterating until you press "Box Counting: Go" again to stop the process (or 'Halt' under "Tools" in the menu bar). Press "Linear Regression" under the plot on the left side of the interface to determine the box-counting-dimension of the image. 

To run this model more quickly, increase the speed slider at the top of the interface to its maximum value.  

## Uploading Images

You can use your own images in this model.  Your image file needs to be in .png format, have square dimension, and be called "custom-image.png".   You also need to identify the color of the pixels that you want box-counting to measure, and find the corresponding color value from the Netlogo Color Swatches in the Tools menu.   The easiest way to do this is to make the color of interest black, and set color-value to 0.  Make sure that the image is square and has no black borders; the only black in the image should be part of the figure itself. Note that the larger the image, the longer it will take to run Image Setup.   

## CREDITS AND REFERENCES

This model is part of the Fractals series of the Complexity Explorer project.  
 
Main Author:  John Driscoll

Contributions from:  Vicki Niu, Melanie Mitchell

## HOW TO CITE

If you use this model, please cite it as: Applied Box Counting model, Complexity Explorer project, http://complexityexplorer.org

## COPYRIGHT AND LICENSE

Copyright 2016 Santa Fe Institute.

This model is licensed by the Creative Commons Attribution-NonCommercial-ShareAlike International (http://creativecommons.org/licenses/). This states that you may copy, distribute, and transmit the work under the condition that you give attribution to ComplexityExplorer.org, and your use is for non-commercial purposes.
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

square
false
0
Rectangle -7500403 true true -27 -62 340 325
Rectangle -7500403 true true 15 45 15 60
Rectangle -7500403 true true -15 -153 417 360

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

x
false
0
Polygon -7500403 true true 270 75 225 30 30 225 75 270
Polygon -7500403 true true 30 75 75 30 270 225 225 270
@#$#@#$#@
NetLogo 6.2.2
@#$#@#$#@
ballSetup
repeat 14 [ go ]
@#$#@#$#@
@#$#@#$#@
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
