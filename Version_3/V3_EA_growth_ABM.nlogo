extensions [ sr rnd csv ]

__includes ["V3_nls/emd.nls" "V3_nls/sweep_function.nls"]

breed [ leaders leader ] ; define an agent type for leaders
breed [ eas ea] ; define an agent type for people in the movement
breed [ outsiders outsider ] ; define an agent type for people not in the movement

; variable spaces

globals [
  conversion-rate ; the proportion of outsiders joining EA
  desertion-rate  ; the proportion of eas leaving EA
  pop-signal
  signal-options  ; possible signals
  ; sweeping helpers
  sweeping? ; true/false variable initiating the model correctly
] ; variables that are the same for the whole space

turtles-own [ ; agent-specific variables - regardless of types
  perceived-ea-alignment
  value-profile ; a letter grade reflecting EA alignment (a normally distributed trait)
  signal-weights ; for repeated signal generation
  signals ; a list of 5 binary signals that can either be EA-aligned (1) or not (0)
  time-as-type ; how long an agent has been its current type

  test1
  test2
  test3
  test4
  test5
  test6
]

; currently only outsiders have special attributes
;leaders-own [
;  group-representation
;]
eas-own [
  deserted?
]

outsiders-own [
  recruited? ; logical variable indicating whether a non-ea'er has been convinced to join the movement
]

links-own [
  perceived-alignment ; an index [0,1] estimated with signals where 0 indicates no alignment and 1 indicates complete alignment
  perceived-misalignment
  actual-alignment ;  a binary {0, 1} estimated with categories where 0 different value profiles and 1 indiciates the same value profile
]

; define model initiation
; 1. spawn agents of all types and define space
; 2. assign agent value category dependent on their type
; 3. generate initial agent signals from value category
; 4. initial alignment estimation from signals
to setup
  ; clear last sim - since globals are just params and sweeping? we do not need to clear those
  clear-turtles
  clear-links
  reset-ticks
  clear-patches

  ; number of ea agents is a proportion of the population corresponding to movement saturation. since the leader is generated separately, we subtract 1.
  let n-eas ceiling ( ( n-agents * movement-saturation / 100 ) ) - 1
  ; number of outsiders is the remainder
  let n-outsiders n-agents - n-eas - 1

  ; agents will form a circle, this will allow them to form a circle with the same spacing between them, depending on the number of agents=
  let ea-radius n-eas / (2 * pi)
  let outsider-radius ea-radius + (n-outsiders / (2 * pi))  ; Add gap between circles

  ; generate model space
  setup-space outsider-radius ea-radius

  ; generate agents and spatial and value properties
  setup-agents n-eas n-outsiders outsider-radius ea-radius

  ; generate alignment-based links and set link properties
  setup-links

  update-plots
end

; function to size the world approrpiately for the number of agents and set the background colour
to setup-space [ outsider-radius ea-radius ]
  ; Ensure minimum radii to avoid crowding
  set ea-radius max list ea-radius 5
  set outsider-radius max list outsider-radius (ea-radius + 8)

  ; size the world to fit the outer circle
  let world-size ceiling ( outsider-radius )
  resize-world ( - world-size ) world-size ( - world-size ) world-size

  ; white background
  ask patches [ set pcolor 9.9 ]
end

; function that initiates agents and their value properties
to setup-agents [ n-eas n-outsiders outsider-radius ea-radius ]
  ; load R package if needed
  if signal-representation = "categorical-dirichlet" and not sweeping? [
    sr:setup
    sr:run "library(MCMCpack)"
  ]
  ; generate agents (without positioning in space yet)
  generate-agents n-eas n-outsiders outsider-radius ea-radius

  generate-values ; assign value profiles

  ask turtles [
    set signals generate-signals n-signals ; generate signals probabilistically from value profile
    create-links-with other turtles ; connect everyone for alignment estimation
    set time-as-type 0
  ]
end

to setup-links
  ask links [
    infer-alignment ; make signal-based inference of alignment
    set thickness perceived-alignment
    if not show-links [ hide-link ]
  ]

  ; compute initial perceived-ea-alignment for all agents
  ask eas [ compute-ea-alignment ]
  ask outsiders [ compute-ea-alignment ]
  ask leaders [ set perceived-ea-alignment 1.0 ] ; leaders have perfect alignment
end

; function generating agents
to generate-agents [ n-eas n-outsiders outsider-radius ea-radius ]
  ; create EA agents
  create-ordered-eas ( n-eas ) [
    fd ea-radius ; place in a circle
    set color blue ; colour each EA agent blue
  ]

  ; create one EA-leader
  create-leaders 1 [
    set color 103
  ]

  ; create people outside the movement
  create-ordered-outsiders n-outsiders [
    fd outsider-radius
    set color pink
  ]
end

to generate-values
  ; Generate normally distributed values for entire population
  let population-values []

  repeat n-agents [
    set population-values lput (random-normal 0 1) population-values
  ]
  ; Sort values in descending order (highest first)
  set population-values sort-by > population-values

  ; Create list of all available indices
  let population-indices n-values length population-values [ i -> i ]

  ; assign leaders value-profile
  ask leaders [
    ; Leaders have strong bias toward high values
    let selected-index select-weighted-index 3.0 population-indices population-values ; high bias parameter
    set value-profile item selected-index population-values
    set value-profile value-category value-profile

    ; Remove this index from available pool
    set population-indices remove selected-index population-indices
  ]

  ; assign eas value-profiles
  ask eas [
    ; EAs have moderate bias toward high values
    let selected-index select-weighted-index 2.5 population-indices population-values ; moderate bias
    set value-profile item selected-index population-values
    set value-profile value-category value-profile

    ; Remove this index from available pool
    set population-indices remove selected-index population-indices
  ]

  ; assign outsiders value-profiles
  ask outsiders [
    ; Outsiders get remaining values (representing general population)
    let selected-index one-of population-indices
    set value-profile item selected-index population-values
    set value-profile value-category value-profile

    ; Remove this index from available pool
    set population-indices remove selected-index population-indices
    ; outsiders are initially not recruited
    set recruited? false
  ]

  ifelse signal-representation = "categorical-dirichlet" [
    ask turtles [
      set signal-weights random-dirichlet 1 (
        ifelse-value
        value-profile = "A" [ ( list 8 7 3 1.5 0.5 ) ]
        value-profile = "B" [ ( list 4.5 6 5.5 3.25 0.75 ) ]
        value-profile = "C" [ ( list 1 5 8 5 1 ) ]
        value-profile = "D" [ ( list 0.75 3.25 5.5 6 4.5 ) ]
        [ ( list 0.5 1.5 3 7 8 ) ]
      ) 20
    ]
    set signal-options ( list 1 2 3 4 5 )
  ] [
    ask turtles [
      set signal-weights item ( position value-profile [ "A" "B" "C" "D" "E" ] ) ( list [ 0.1 0.9 ] [ 0.3 0.7 ] [ 0.5 0.5 ] [ 0.7 0.3 ] [ 0.9 0.1 ] )
    ]
    set signal-options ( list 0 1 )
  ]
end

; function that creates a set of signals dependent on value profile
to-report generate-signals [ n ]
  let signal ( n-values n [ weighted-prob-draw signal-weights signal-options ] )

  report signal
end

to infer-alignment
  let signal1 [signals] of end1
  let signal2 [signals] of end2

  ifelse signal-representation = "categorical-dirichlet" [
    set perceived-misalignment emd-distance signal1 signal2 "dir" ; get emd directional adjustments
    set perceived-alignment emd-distance signal1 signal2 "sim" ; get emd similarity
  ] [
    set perceived-misalignment ( sum signal2  - sum signal1 ) / length ( signal1 )
    set perceived-alignment 1 - ( abs ( perceived-misalignment ) )
    ;set perceived-alignment ( perceived-alignment - 0.5 ) * 2
  ]

  set actual-alignment ifelse-value ( [ value-profile ] of end1 = [ value-profile ] of end2 ) [ 1 ] [ 0 ]
end

to go
  re-assign-groups
  update-links
  ifelse signal-representation = "categorical-dirichlet"
  [ update-ea-values-cat ]
  [ update-ea-values-binary]
  recruit-eaers
  desert-eaers
  update-agents
  tick
end

to compute-ea-alignment
  let ea-leader-alignment first [ perceived-alignment ] of my-links with [ member? other-end leaders ]
  let alignments [perceived-alignment] of my-links with [ member? other-end eas]
  let avg-ea-alignment 0

  if length alignments > 0 [
    set avg-ea-alignment mean alignments
  ]
  let weighted-leader-sim ea-leader-alignment * ( relative-leader-influence / 100 * 2 )

  let weighted-avg-sim avg-ea-alignment * ( ( 1 - ( relative-leader-influence / 100 ) ) * 2 )

  let elements (list weighted-leader-sim weighted-avg-sim)

  set perceived-ea-alignment ifelse-value aggregation-method = "average" [ ( sum elements ) / length elements ] [ reduce * elements ]
end

to recruit-eaers
  ask outsiders [
    compute-ea-alignment

    set recruited? random-float 1 <  ( perceived-ea-alignment ^ ( 100 ^ log-recruitment-strictness ) )

    if recruited? [
      set color blue + 2
    ]
  ]

  set conversion-rate get-conversion-rate
end

to desert-eaers
  ask eas [
    compute-ea-alignment

    set deserted? random-float 1 < ( 1 - ( perceived-ea-alignment ^ ( 100 ^ log-desertion-strictness ) ) )

    if deserted? [
      set color pink + 2
    ]
  ]
  set desertion-rate get-desertion-rate
end

to re-assign-groups
  ; Convert recruited outsiders to eas
  ask outsiders with [recruited? = true] [
    ; Store current properties
    let my-xcor xcor
    let my-ycor ycor
    let my-color color
    let my-value-profile value-profile
    let my-signals signals
    let my-test1 test1
    let my-test2 test2
    let my-test3 test3
    let my-test4 test4
    let my-perceived-ea-alignment perceived-ea-alignment

    ; Create new ea agent
    hatch-eas 1 [
      set xcor my-xcor
      set ycor my-ycor
      set color my-color
      set value-profile my-value-profile
      set signals my-signals
      set test1 my-test1
      set test2 my-test2
      set test3 my-test3
      set test4 my-test4
      set perceived-ea-alignment my-perceived-ea-alignment
      set time-as-type 0
      set deserted? false  ; initialize as not deserted
      set size 1

      ; Create undirected links with all other agents
      create-links-with other turtles
    ]
    die
  ]

  ; Convert deserted eas to outsiders
  ask eas with [deserted? = true] [
    ; Store current properties
    let my-xcor xcor
    let my-ycor ycor
    let my-color color
    let my-value-profile value-profile
    let my-signals signals
    let my-test1 test1
    let my-test2 test2
    let my-test3 test3
    let my-test4 test4
    let my-perceived-ea-alignment perceived-ea-alignment

    ; Create new outsider agent
    hatch-outsiders 1 [
      set xcor my-xcor
      set ycor my-ycor
      set color my-color
      set value-profile my-value-profile
      set signals my-signals
      set test1 my-test1
      set test2 my-test2
      set test3 my-test3
      set test4 my-test4
      set perceived-ea-alignment my-perceived-ea-alignment
      set time-as-type 0
      set recruited? false  ; initialize as not recruited
      set size 1

      ; Create undirected links with all other agents
      create-links-with other turtles
    ]
    die
  ]
end

to update-links
  ask links [
    infer-alignment
    set thickness perceived-alignment
    if not show-links [ hide-link ]
  ]
end

to update-agents
  ifelse refresh-level = "population" [
    if random-float 1 < ( new-signal-rate / 100 ) [
      set pop-signal true
      ask turtles [
      set signals lput ( first generate-signals 1 ) but-first signals
      ]
    ]
  ] [
    ask turtles [
      if random-float 1 < ( new-signal-rate / 100 ) [
         set signals lput ( first generate-signals 1 ) but-first signals
      ]
    ]
  ]

  ask turtles [ set time-as-type time-as-type + 1 ]
end

to-report compute-ea-misalignment-binary
  ; Get misalignment values, adjusting sign based on agent's position in the link
  let ea-leader-misalignment 0
  let ea-misalignments []

  ; For leader misalignment
  ask my-links with [ member? other-end leaders ] [
    let raw-misalignment perceived-misalignment
    ; If current agent is end2, reverse the sign
    ifelse [who] of myself = [who] of end2 [
      set ea-leader-misalignment (- raw-misalignment)
    ][
      set ea-leader-misalignment raw-misalignment
    ]
  ]

  ; For EA misalignments
  ask my-links with [ member? other-end eas ] [
    let raw-misalignment perceived-misalignment
    let adjusted-misalignment raw-misalignment
    ; If current agent is end2, reverse the sign
    if [who] of myself = [who] of end2 [
      set adjusted-misalignment (- raw-misalignment)
    ]
    set ea-misalignments lput adjusted-misalignment ea-misalignments
  ]
  ; if there are no alignments we set a 0
  if empty? ea-misalignments [ set ea-misalignments  lput 0 ea-misalignments ]

  let avg-ea-misalignment mean ea-misalignments

  ; Weight the components
  let weighted-leader-misalign ea-leader-misalignment * ( relative-leader-influence / 100 * 2 )
  let weighted-avg-misalign avg-ea-misalignment * ( ( 1 - ( relative-leader-influence / 100 ) ) * 2 )

  let elements (list weighted-leader-misalign weighted-avg-misalign)

  ; Calculate final misalignment
  let perceived-ea-misalignment ifelse-value aggregation-method = "average" [
    mean elements
  ][
    reduce * elements
  ]

  report perceived-ea-misalignment
end

to-report compute-ea-misalignment-cat
  let ea-leader-misalignment-vector n-values (length signal-options) [0]
  let ea-misalignment-vectors []

  ; For leader misalignment vector
  ask my-links with [ member? other-end leaders ] [
    let raw-misalignment-vector perceived-misalignment
    ; If current agent is end2, reverse the sign of the entire vector
    ifelse [who] of myself = [who] of end2 [
      set ea-leader-misalignment-vector map [ x -> (- x) ] raw-misalignment-vector
    ][
      set ea-leader-misalignment-vector raw-misalignment-vector
    ]
  ]
  ; For EA misalignments
  ask my-links with [ member? other-end eas ] [
    let raw-misalignment-vector perceived-misalignment
    let adjusted-misalignment-vector raw-misalignment-vector
    ; If current agent is end2, reverse the sign of the entire vector
    if [who] of myself = [who] of end2 [
      set adjusted-misalignment-vector map [ x -> (- x) ] raw-misalignment-vector
    ]
      set ea-misalignment-vectors lput adjusted-misalignment-vector ea-misalignment-vectors
  ]

  ; if there are no member alignments, set the vector to perfect alignment
  if empty? ea-misalignment-vectors  [
   set ea-misalignment-vectors lput ( n-values ( length signal-options ) [ 0 ] ) ea-misalignment-vectors
  ]

  ; Calculate element-wise mean across all EA misalignment vectors
  let avg-ea-misalignment-vector n-values ( length signal-options ) [ 0 ]
  if length ea-misalignment-vectors > 0 [
    set avg-ea-misalignment-vector map [ i ->
      mean (map [ vec -> item i vec ] ea-misalignment-vectors)
    ] (range ( length signal-options ) )
  ]

  ; Weight the components element-wise
  let weighted-leader-misalign map [ x -> x * ( relative-leader-influence / 100 )] ea-leader-misalignment-vector
  let weighted-avg-misalign map [ x -> x * ( 1 - ( relative-leader-influence / 100 ) ) ] avg-ea-misalignment-vector

  ; Calculate final misalignment vector
  let perceived-ea-misalignment-vector ifelse-value aggregation-method = "average" [
    ; Combine weighted components element-wise (addition for average)
    ( map [ [a b] -> ( a + b ) ] weighted-leader-misalign weighted-avg-misalign )
  ][
    ; Combine weighted components element-wise (multiplication for product)
    ( map [ [a b] -> a * b * 2] weighted-leader-misalign weighted-avg-misalign )
  ]

  set test1 perceived-ea-misalignment-vector
  report perceived-ea-misalignment-vector
end

to update-ea-values-binary ;; ONLY WORKS ON BINARY SIGNAL REPRESENTATIONS
  ask eas [
    let update compute-ea-misalignment-binary

    ifelse value-function = "proportional" [
      let w1-updated ( item 1 signal-weights ) + update * social-influence

      set w1-updated max ( list 0 ( min list 1 w1-updated ) )

      let w0-updated 1 - w1-updated

      set signal-weights list w0-updated w1-updated
    ][
      let w1 item 1 signal-weights

      ;set w1 max ( list 0.001 ( min list w1 0.999 ) )

      let log-odds  ( ln ( w1 / ( 1 - w1 ) ) ) + update * social-influence

      let w1-updated exp log-odds / ( 1 + exp log-odds )

      let w0-updated 1 - w1-updated

      set signal-weights list w0-updated w1-updated
    ]
  ]
end

to update-ea-values-cat ;; ONLY WORKS ON CATEGORICAL SIGNAL REPRESENTATIONS
  ask eas [
    let update-vector compute-ea-misalignment-cat
    let old-weights signal-weights

    ; Logistic approach using softmax
    ; Clamp current weights to avoid log(0)
    let clamped-weights map [ w -> max (list 0.001 (min (list 0.999 w))) ] signal-weights

    ; Convert to log space
    let log-weights map [ w -> ln w ] clamped-weights

    ; Apply updates in log space
    let updated-log-weights ( map [ [lw u] -> lw + (u * social-influence ) ] log-weights update-vector )

    ; Convert back via softmax (automatically normalizes to sum = 1)
    let max-log-weight max updated-log-weights  ; For numerical stability
    let exp-weights map [ lw -> exp (lw - max-log-weight) ] updated-log-weights
    let exp-sum sum exp-weights
    let new-weights map [ ew -> ew / exp-sum ] exp-weights

    set signal-weights new-weights
  ]
end

; Supporting reporters
to-report select-weighted-index [ bias-strength population-indices population-values ] ; Creates weights based on trait values, with bias toward higher values
  let weights []
  foreach population-indices [ index ->
    let trait-value item index population-values
    ; Transform trait value to positive weight, with bias
    let weight exp(bias-strength * trait-value)
    set weights lput weight weights
  ]

  ; Select index using weighted probability
  let pairs (map list population-indices weights)
  report first rnd:weighted-one-of-list pairs [ [p] -> last p ]
end

to-report value-category [ cont-val ] ; returns a value-profile from a continuous value
  ; Cutpoints based on normal distribution quantiles
  ; These create: A(~7%), B(~24%), C(~38%), D(~24%), E(~7%)
  report ( ifelse-value
    cont-val > 1.5 [ "A" ]      ; ~93rd percentile and above
    cont-val > 0.5 [ "B" ]      ; ~69th to 93rd percentile
    cont-val > -0.5 [ "C" ]     ; ~31st to 69th percentile
    cont-val > -1.5 [ "D" ]     ; ~7th to 31st percentile
    [ "E" ]                     ; Below ~7th percentile
  )
end

to-report weighted-prob-draw [prob-list option-list]     ; generates an outcome from a set of probabilities and their associated outcomes
  let pairs (map list option-list prob-list)
  report first rnd:weighted-one-of-list pairs [ [p] -> last p ]
end

to-report get-conversion-rate  ; estimates the proportion of outsiders joining EA at the end of the sim
  if count outsiders = 0 [ report 0 ]  ; Avoid division by zero
  report count outsiders with [ recruited? ] / count outsiders
end

to-report get-desertion-rate
   if count eas = 0 [ report 0 ]  ; Avoid division by zero
  report count eas with [ deserted? ] / count eas
end

to-report random-dirichlet [ n alpha concentration ]
  sr:set "n" n
  sr:set "c" concentration
  sr:set "a" alpha

  let vals sr:runresult "rdirichlet(n, a*c)"
  report vals
end
@#$#@#$#@
GRAPHICS-WINDOW
932
49
1369
487
-1
-1
13.0
1
10
1
1
1
0
0
0
1
-16
16
-16
16
1
1
1
ticks
30.0

SLIDER
485
46
722
79
movement-saturation
movement-saturation
0
50
10.0
1
1
%
HORIZONTAL

INPUTBOX
0
10
73
78
n-agents
100.0
1
0
Number

MONITOR
822
460
927
505
n-eas
count eas
17
1
11

PLOT
310
549
617
682
EA value profile distribution
Value Profiles A-E
N agents
1.0
6.0
0.0
5.0
true
false
"" ""
PENS
"EAs" 1.0 1 -13345367 true "clear-plot\nplotxy 1 (count eas with [value-profile = \"A\"])\nplotxy 2 (count eas with [value-profile = \"B\"])  \nplotxy 3 (count eas with [value-profile = \"C\"])\nplotxy 4 (count eas with [value-profile = \"D\"])\nplotxy 5 (count eas with [value-profile = \"E\"])\nplotxy 5 (count eas with [value-profile = \"E\"])" "clear-plot\nplotxy 1 (count eas with [value-profile = \"A\"])\nplotxy 2 (count eas with [value-profile = \"B\"])  \nplotxy 3 (count eas with [value-profile = \"C\"])\nplotxy 4 (count eas with [value-profile = \"D\"])\nplotxy 5 (count eas with [value-profile = \"E\"])\nplotxy 5 (count eas with [value-profile = \"E\"])"

PLOT
619
549
924
682
Non-EA value profile distribution
Value Profiles A-E
N agents
1.0
6.0
0.0
5.0
true
false
"" ""
PENS
"default" 1.0 1 -13840069 true "clear-plot\nplotxy 1 (count outsiders with [value-profile = \"A\"])\nplotxy 2 (count outsiders with [value-profile = \"B\"])  \nplotxy 3 (count outsiders with [value-profile = \"C\"])\nplotxy 4 (count outsiders with [value-profile = \"D\"])\nplotxy 5 (count outsiders with [value-profile = \"E\"])" "clear-plot\nplotxy 1 (count outsiders with [value-profile = \"A\"])\nplotxy 2 (count outsiders with [value-profile = \"B\"])  \nplotxy 3 (count outsiders with [value-profile = \"C\"])\nplotxy 4 (count outsiders with [value-profile = \"D\"])\nplotxy 5 (count outsiders with [value-profile = \"E\"])"

MONITOR
822
412
927
457
conversion rate
conversion-rate
2
1
11

SWITCH
140
10
238
43
show-links
show-links
1
1
-1000

CHOOSER
245
82
411
127
aggregation-method
aggregation-method
"average" "product"
1

CHOOSER
415
82
580
127
signal-representation
signal-representation
"binary" "categorical-dirichlet"
1

SLIDER
241
46
481
79
relative-leader-influence
relative-leader-influence
0
100
66.0
1
1
%
HORIZONTAL

PLOT
0
549
306
683
Population value profile distribution
Value Profiles A-E
N agents
1.0
6.0
0.0
5.0
true
false
"" ""
PENS
"default" 1.0 1 -11221820 true "clear-plot\nplotxy 1 (count turtles with [value-profile = \"A\"])\nplotxy 2 (count turtles with [value-profile = \"B\"])  \nplotxy 3 (count turtles with [value-profile = \"C\"])\nplotxy 4 (count turtles with [value-profile = \"D\"])\nplotxy 5 (count turtles with [value-profile = \"E\"])" "clear-plot\nplotxy 1 (count turtles with [value-profile = \"A\"])\nplotxy 2 (count turtles with [value-profile = \"B\"])  \nplotxy 3 (count turtles with [value-profile = \"C\"])\nplotxy 4 (count turtles with [value-profile = \"D\"])\nplotxy 5 (count turtles with [value-profile = \"E\"])"

SLIDER
483
10
721
43
log-recruitment-strictness
log-recruitment-strictness
-1
1
0.5
0.1
1
NIL
HORIZONTAL

SLIDER
240
10
479
43
log-desertion-strictness
log-desertion-strictness
-1
1
-0.5
0.1
1
NIL
HORIZONTAL

BUTTON
0
81
67
126
setup
set sweeping? false\nset pop-signal false\nsetup
NIL
1
T
OBSERVER
NIL
S
NIL
NIL
1

BUTTON
69
81
154
126
Go once
set pop-signal false\ngo
NIL
1
T
OBSERVER
NIL
O
NIL
NIL
0

BUTTON
158
82
242
126
Go
set pop-signal false\ngo
T
1
T
OBSERVER
NIL
G
NIL
NIL
0

MONITOR
823
361
926
406
desertion rate
desertion-rate
2
1
11

PLOT
439
358
820
548
Number of EAs and non-EAs
Time
N agents
0.0
10.0
0.0
10.0
true
false
"clear-plot" ""
PENS
"eas" 1.0 0 -13345367 true "" "plot count eas"
"non-eas" 1.0 0 -2064490 true "" "plot count outsiders"
"signal alert" 1.0 2 -2674135 false "" "if refresh-level = \"population\" and pop-signal [plotxy ticks 1]"

PLOT
440
143
928
359
Conversion and desertion rate
time
rate
0.0
10.0
-0.1
0.1
true
true
"clear-plot" ""
PENS
"desertion" 1.0 0 -1264960 true "" "plot desertion-rate"
"recruitment" 1.0 0 -8275240 true "" "plot conversion-rate"
"net-recruitment" 1.0 0 -5204280 true "" "plot conversion-rate - desertion-rate"
"break-even" 1.0 0 -7500403 false "" "plot 0"
"signal alert" 1.0 2 -2674135 false "" "if refresh-level = \"population\" and pop-signal [plotxy ticks 0.9]"

SLIDER
725
10
913
43
new-signal-rate
new-signal-rate
0
100
25.0
5
1
%
HORIZONTAL

PLOT
0
360
437
548
Mean Age by Value Profile
Time
Age
0.0
10.0
0.0
5.0
true
true
"clear-plot" ""
PENS
"All" 1.0 0 -16777216 true "" "plot mean [time-as-type] of turtles"
"A" 1.0 0 -2674135 true "" "plot mean [time-as-type] of turtles with [value-profile = \"A\"]"
"B" 1.0 0 -955883 true "" "plot mean [time-as-type] of turtles with [value-profile = \"B\"]"
"C" 1.0 0 -1184463 true "" "plot mean [time-as-type] of turtles with [value-profile = \"C\"]"
"D" 1.0 0 -13840069 true "" "plot mean [time-as-type] of turtles with [value-profile = \"D\"]"
"E" 1.0 0 -11221820 true "" "plot mean [time-as-type] of turtles with [value-profile = \"E\"]"

PLOT
0
143
443
359
Mean age by type
Time
Age
0.0
10.0
0.0
10.0
true
true
"clear-plot" ""
PENS
"All" 1.0 0 -16777216 true "" "plot mean [time-as-type] of turtles"
"EAs" 1.0 0 -13345367 true "" "plot mean [time-as-type] of eas"
"Non-EAs" 1.0 0 -2064490 true "" "plot mean [time-as-type] of outsiders"
"signal alert" 1.0 2 -2674135 false "" "if refresh-level = \"population\" and pop-signal [plotxy ticks 1]"

CHOOSER
752
84
913
129
refresh-level
refresh-level
"population" "agent"
0

SLIDER
726
47
913
80
social-influence
social-influence
0
5
0.0
0.2
1
NIL
HORIZONTAL

CHOOSER
584
83
748
128
value-function
value-function
"proportional" "logistic"
1

PLOT
934
499
1406
731
EA Values over Time
Time
Signal Probability
0.0
10.0
0.0
1.0
true
true
"clear-plot" ""
PENS
"P(signal= 0)" 1.0 0 -2064490 true "" "if signal-representation = \"binary\"[ plot (( sum ( [ item 0 signal-weights] of eas) ) + ( sum ( [ item 0 signal-weights] of leaders) )) / (count eas + count leaders)]"
"P(signal = 1)" 1.0 0 -13345367 true "" "ifelse signal-representation = \"binary\"[\nplot (( sum ( [ item 1 signal-weights] of eas) ) + ( sum ( [ item 1 signal-weights] of leaders ) )) / (count eas + count leaders)\n][\n plot (( sum ( [ item 0 signal-weights] of eas) ) + ( sum ( [ item 0 signal-weights] of leaders) )) / (count eas + count leaders)\n]"
"P(signal = 2)" 1.0 0 -8020277 true "" "if signal-representation = \"categorical-dirichlet\" [plot (( sum ( [ item 1 signal-weights] of eas) ) + ( sum ( [ item 1 signal-weights] of leaders ) )) / (count eas + count leaders)]"
"P(signal = 3)" 1.0 0 -8630108 true "" "if signal-representation = \"categorical-dirichlet\" [plot (( sum ( [ item 2 signal-weights] of eas) ) + ( sum ( [ item 2 signal-weights] of leaders) )) / (count eas + count leaders)]"
"P(signal = 4)" 1.0 0 -1264960 true "" "if signal-representation = \"categorical-dirichlet\" [plot (( sum ( [ item 3 signal-weights] of eas) ) + ( sum ( [ item 3 signal-weights] of leaders) )) / (count eas + count leaders)]"
"P(signal = 5)" 1.0 0 -2064490 true "" "if signal-representation = \"categorical-dirichlet\" [plot (( sum ( [ item 4 signal-weights] of eas) ) + ( sum ( [ item 4 signal-weights] of leaders) )) / (count eas + count leaders)]"

INPUTBOX
78
10
137
78
n-signals
5.0
1
0
Number

PLOT
1411
499
1823
731
Average Perceived Alignment
Time
Mean Perceived Alignment
0.0
10.0
0.0
1.0
true
true
"clear-plot" ""
PENS
"EAs" 1.0 0 -13345367 true "" "plot mean [perceived-ea-alignment] of eas"
"Non-EAs" 1.0 0 -2064490 true "" "plot mean [perceived-ea-alignment] of outsiders"
"All" 1.0 0 -16777216 true "" "plot mean [perceived-ea-alignment] of turtles"
"Min" 1.0 0 -11221820 true "" "plot min ( sentence [perceived-ea-alignment] of eas  [perceived-ea-alignment] of outsiders)"
"Max" 1.0 0 -2674135 true "" "plot max ( sentence [perceived-ea-alignment] of eas  [perceived-ea-alignment] of outsiders)"
"signal-alert" 1.0 2 -2674135 false "" "if refresh-level = \"population\" and pop-signal [plotxy ticks 0.05]"

PLOT
937
735
1407
929
Non-EA Values over time
Time
Signal probability
0.0
10.0
0.0
1.0
true
false
"clear-plot" ""
PENS
"P(signal = 0)" 1.0 0 -2064490 true "" "if signal-representation = \"binary\" [plot mean ( [item 0 signal-weights] of outsiders)]"
"P(signal = 1)" 1.0 0 -13345367 true "" "ifelse signal-representation = \"binary\"[\nplot mean ( [item 1 signal-weights] of outsiders)\n][\nplot mean ( [item 0 signal-weights] of outsiders)\n]"
"P(signal = 2)" 1.0 0 -8020277 true "" "if signal-representation = \"categorical-dirichlet\" [plot mean ( [item 1 signal-weights] of outsiders)]"
"P(signal = 3)" 1.0 0 -8630108 true "" "if signal-representation = \"categorical-dirichlet\" [plot mean ( [item 2 signal-weights] of outsiders)]"
"P(signal = 4)" 1.0 0 -1264960 true "" "if signal-representation = \"categorical-dirichlet\" [plot mean ( [item 3 signal-weights] of outsiders)]"
"P(signal = 5)" 1.0 0 -2064490 true "" "if signal-representation = \"categorical-dirichlet\" [plot mean ( [item 4 signal-weights] of outsiders)]"

@#$#@#$#@
## WHAT IS IT?

(a general understanding of what the model is trying to show or explain)

## HOW IT WORKS

(what rules the agents use to create the overall behavior of the model)

## HOW TO USE IT

(how to use the model, including a description of each of the items in the Interface tab)

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
NetLogo 6.4.0
@#$#@#$#@
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
