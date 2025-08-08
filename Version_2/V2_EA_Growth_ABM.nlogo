extensions [ sr rnd csv ]

breed [ leaders leader ] ; define an agent type for leaders
breed [ eas ea] ; define an agent type for people in the movement
breed [ outsiders outsider ] ; define an agent type for people not in the movement

; variable spaces

globals [
  conversion-rate ; the proportion of outsiders joining EA
  ; sweeping helpers
  sweeping? ; true/false variable initiating the model correctly
] ; variables that are the same for the whole space

turtles-own [ ; agent-specific variables - regardless of types
  value-profile ; a letter grade reflecting EA alignment (a normally distributed trait)
  signals ; a list of 5 binary signals that can either be EA-aligned (1) or not (0)
  test1
  test2
  test3
  test4
]

; currently only outsiders have special attributes
;leaders-own [
;  group-representation
;]
;eas-own [
;  group-representation
;]
outsiders-own [
  recruited? ; logical variable indicating whether a non-ea'er has been convinced to join the movement
  perceived-ea-alignment
]

links-own [
  perceived-alignment ; an index [0,1] estimated with signals where -1 indicates no alignment and 1 indicates complete alignment
  actual-alignment ;  a binary {0, 1} estimated with categories where -1 different value profiles and 1 indiciates the same value profile
]

; define model initiation
; 1. spawn agents of all types and define space
; 2. assign agent value category dependent on their type
; 3. generate initial agent signals from value category
; 4. initial alignment estimation from signals
to setup
  ; clear last sim - since globals are just params and sweeping? we do not need to clear those
  ask turtles [ die ]
  ask links [ die ]
  reset-ticks

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
  ; load R pacakge if needed
  if signal-representation = "categorical-dirichlet" and not sweeping? [
    sr:setup
    sr:run "library(MCMCpack)"
  ]
  ; generate agents and position in space
  generate-agents n-eas n-outsiders outsider-radius ea-radius

  generate-values ; assign value profiles

  ask turtles [
    generate-signals ; generate signals probabilistically from value profile
    create-links-with other turtles ; connect everyone for alignment estimation
  ]
end

to setup-links
  ask links [
    infer-alignment ; make signal-based inference of alignment
    set thickness perceived-alignment
    if not show-links [ hide-link ]
  ]
end

; function generating and positioning agents
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
    set value-profile value-category item selected-index population-values
    ; Remove this index from available pool
    set population-indices remove selected-index population-indices
  ]

  ; assign eas value-profiles
  ask eas [
    ; EAs have moderate bias toward high values
    let selected-index select-weighted-index 1.5 population-indices population-values ; moderate bias
    set value-profile value-category item selected-index population-values
    set population-indices remove selected-index population-indices
  ]

  ; assign outsiders value-profiles
  ask outsiders [
    ; Outsiders get remaining values (representing general population)
    let selected-index one-of population-indices
    set value-profile value-category item selected-index population-values
    set population-indices remove selected-index population-indices
    ; outsiders are initially not recruited
    set recruited? false
  ]
end

; function that creates a set of signals dependent on value profile
to generate-signals
  ifelse signal-representation = "categorical-dirichlet" [
    let weights random-dirichlet 1 (
      ifelse-value
      value-profile = "A" [ ( list 8 7 3 1.5 0.5) ]
      value-profile = "B" [ ( list 4.5 6.0 5.5 3.25 0.75) ]
      value-profile = "C" [ ( list 1 5 8 5 1 ) ]
      value-profile = "D" [ ( list 0.75 3.25 5.5 6 4.5 ) ]
      [ ( list 0.5 1.5 3 7 8 ) ]
    ) 20
    set test1 weights
    set signals n-values 5 [ weighted-prob-draw weights [ 5 4 3 2 1 ] ]
  ] [
    let weights item ( position value-profile [ "A" "B" "C" "D" "E" ] ) [ 0.9 0.7 0.5 0.3 0.1 ]
    set weights list ( 1 - weights ) weights
    set test1 weights
    set signals n-values 5 [ weighted-prob-draw weights [ 0 1 ] ]
  ]
end

to infer-alignment
  let pair-signals [ signals ] of both-ends
  let signal1 item 0 pair-signals
  let signal2 item 1 pair-signals

  ifelse signal-representation = "categorical-dirichlet" [
    set perceived-alignment emd-similarity signal1 signal2
  ] [
    set perceived-alignment 1 - ( abs ( sum signal1  - sum signal2 ) / length ( signal1 ) )
    ;set perceived-alignment ( perceived-alignment - 0.5 ) * 2
  ]

  set actual-alignment ifelse-value ( [ value-profile ] of end1 = [ value-profile ] of end2 ) [ 1 ] [ 0 ]
end

to go
  recruit-eaers
  tick
  update-plots
end

to sweep [ param-combo ]
  ; Initialize sweep by clearing everything and setting up parameter ranges
  clear-all
  set sweeping? true

  ; setting up parameters according to sweep-params and asks for user approval
  ; Define variables for custom parameters
  let aggregation-methods [ "average" "product" ]
  let signal-representations [ "binary" "categorical-dirichlet" ]
  let leader-influence-values ( range 0 101 5 )  ; Relative leader influence from 0% to 100% in steps of 5
  let runs-per-combo 5
  let m-sat-values 0
  let n-agent-values 0

  ; define ranges based on param-combo
  ( ifelse param-combo = "focused-population" [
    set m-sat-values ( range 5 96 5 )  ; Movement saturation from 5% to 95% in steps of 5
    set n-agent-values [ 100 1000 ]  ; Different population sizes to test
  ] param-combo = "test-range" [
    set m-sat-values ( range 5 96 5 )  ; Movement saturation from 5% to 95% in steps of 5
    set n-agent-values [ 25 50 100 ]  ; Different population sizes to test
  ] param-combo = "custom-all" [
    ; Get custom parameters from user
    let m-sat-input user-input "Enter movement saturation values (e.g., 5 15 25 35 45 55 65 75 85 95):"
    let n-agent-input user-input "Enter population sizes (e.g., 25 50 100 250 500 1000):"
    let agg-method-input user-input "Enter aggregation methods (e.g., 0 = average 1 = product):"
    let signal-rep-input user-input "Enter signal representations (e.g., 0 = binary 1 = categorical-dirichlet):"
    let leader-influence-input user-input "Enter relative leader influence values (e.g., 0 25 50 75 100):"
    let runs-input user-input "Enter number of runs per combination:"

    ; Parse the inputs
    set m-sat-values read-from-string (word "[" m-sat-input "]")
    set n-agent-values read-from-string (word "[" n-agent-input "]")
    set aggregation-methods map [ i -> ( ifelse-value i = 0 [ "average" ] [ "product" ] ) ] read-from-string (word "[" agg-method-input "]")
    set signal-representations map [i -> ifelse-value i = 1 [ "categorical-dirichlet" ] [ "binary" ] ] read-from-string (word "[" signal-rep-input "]")
    set leader-influence-values read-from-string (word "[" leader-influence-input "]")
    set runs-per-combo read-from-string runs-input
  ] [
    ; Default case (full-range)
    set m-sat-values ( range 5 96 5 )  ; Movement saturation from 5% to 95% in steps of 5
    set n-agent-values [ 25 50 100 250 500 1000 ]  ; Different population sizes to test
    ]
  )

  ; Check with user before starting the sweep
  let total-combinations (length m-sat-values * length n-agent-values * length aggregation-methods * length signal-representations * length leader-influence-values * runs-per-combo)
  let proceed? user-yes-or-no? (word
    "Start parameter sweep with the following settings?\n\n"
    "Movement saturation: " m-sat-values "\n"
    "Population sizes: " n-agent-values "\n"
    "Aggregation methods: " aggregation-methods "\n"
    "Signal representations: " signal-representations "\n"
    "Relative leader influence: " leader-influence-values "\n"
    "Runs per combination: " runs-per-combo "\n"
    "Total simulations: " total-combinations "\n\n"
    "This may take several minutes. Continue?" )

  ; Exit if user chooses not proceed
  if not proceed? [
    user-message "Sweep cancelled."
    stop
  ]

  if member? "categorical-dirichlet" signal-representations [
    sr:setup
    sr:run "library(MCMCpack)"
  ]

  ; Clear any existing plot data to start fresh
  clear-all-plots

  ; Define distinct colors for each combination
  let pen-colors [ red blue green orange violet cyan magenta yellow brown pink ]
  let color-index 0

  ; Initialize data collection lists for R plotting
  let all-data []  ; Will store: [n-agents movement-saturation relative-leader-influence conversion-rate run-id aggregation-method signal-representation]

  ; Loop through each population size
  foreach n-agent-values [ n ->

    ; Loop through each aggregation method
    foreach aggregation-methods [ agg-method ->

      ; Loop through each signal representation
      foreach signal-representations [ sig-rep ->

        ; Loop through each relative leader influence level
        foreach leader-influence-values [ leader-inf ->

          ; Create a pen name
          let pen-name (word "n=" n " " agg-method " " sig-rep " leader=" leader-inf)

          ; Create a pen for individual simulation runs
          set-current-plot "Conversion Rate by Movement Saturation"
          create-temporary-plot-pen pen-name
          set-plot-pen-color item (color-index mod length pen-colors) pen-colors

          ; Create a pen for means in the separate means plot
          set-current-plot "Mean Conversion Rates"
          create-temporary-plot-pen pen-name
          set-plot-pen-color item (color-index mod length pen-colors) pen-colors

          set color-index color-index + 1

          ; Loop through each movement saturation level
          foreach m-sat-values [m ->
            ; Set parameters for this combination
            set n-agents n
            set movement-saturation m
            set aggregation-method agg-method
            set signal-representation sig-rep
            set relative-leader-influence leader-inf

            let cr-values []  ; List to store conversion rates from multiple runs

            ; Switch to individual runs pen for plotting
            set-current-plot "Conversion Rate by Movement Saturation"
            set-current-plot-pen pen-name

            ; Run the simulation multiple times with the same parameters
            let run-id 0
            repeat runs-per-combo [
              set run-id run-id + 1
              setup  ; Initialize model with current parameters
              go     ; Run one simulation step

              plotxy m conversion-rate  ; Plot individual result
              set cr-values lput conversion-rate cr-values  ; Add to list for mean calculation

              ; count number of converted turtles by profile
              let converted-A count outsiders with [ recruited? and value-profile = "A" ]
              let converted-B count outsiders with [ recruited? and value-profile = "B" ]
              let converted-C count outsiders with [ recruited? and value-profile = "C" ]
              let converted-D count outsiders with [ recruited? and value-profile = "D" ]
              let converted-E count outsiders with [ recruited? and value-profile = "E" ]

              ; total count by profile
              let total-A count outsiders with [ value-profile = "A" ]
              let total-B count outsiders with [ value-profile = "B" ]
              let total-C count outsiders with [ value-profile = "C" ]
              let total-D count outsiders with [ value-profile = "D" ]
              let total-E count outsiders with [ value-profile = "E" ]

               ; convertion rates by profile
              let conversion-rate-A 0
              let conversion-rate-B 0
              let conversion-rate-C 0
              let conversion-rate-D 0
              let conversion-rate-E 0

              carefully[ set conversion-rate-A converted-A / total-A ][ set conversion-rate-A "NA" ]
              carefully[ set conversion-rate-B converted-B / total-B ][ set conversion-rate-B "NA" ]
              carefully[ set conversion-rate-C converted-C / total-C ][ set conversion-rate-C "NA" ]
              carefully[ set conversion-rate-D converted-D / total-D ][ set conversion-rate-D "NA" ]
              carefully[ set conversion-rate-E converted-E / total-E ][ set conversion-rate-E "NA" ]

              ; Store data for R plotting: [n-agents movement-saturation relative-leader-influence conversion-rate run-id aggregation-method signal-representation]
              set all-data lput ( list run-id n m leader-inf agg-method sig-rep conversion-rate
                conversion-rate-A conversion-rate-B conversion-rate-C conversion-rate-D conversion-rate-E
                converted-A converted-B converted-C converted-D converted-E
                total-A total-B total-C total-D total-E ) all-data
            ]

            ; Calculate mean of the runs and plot in the means plot
            let mean-cr mean cr-values
            set-current-plot "Mean Conversion Rates"
            set-current-plot-pen pen-name
            plotxy m mean-cr
          ]

          ; Progress indicator for this combination
          print ( word "Completed n-agents = " n ", method = " agg-method ", signal-rep = " sig-rep ", leader-influence = " leader-inf)
        ]
      ]
    ]
  ]

  ; Reset sweep mode
  set sweeping? false

  export-sweep-data all-data
  user-message "Sweep complete! All parameter combinations have been tested. Data exported to 'sweep_data.csv'."
end

to recruit-eaers
  ask outsiders [
    let ea-leader-alignment first [ perceived-alignment ] of my-links with [ member? other-end leaders ]
    let avg-ea-alignment mean [perceived-alignment] of my-links with [ member? other-end eas]

    let weighted-leader-sim ea-leader-alignment * ( relative-leader-influence / 100 * 2 )
    let weighted-avg-sim avg-ea-alignment * ( ( 1 - ( relative-leader-influence / 100 ) ) * 2 )

    let elements (list weighted-leader-sim weighted-avg-sim)
    if include-movement-saturation? [ set elements lput (movement-saturation / 100) elements ]

    set perceived-ea-alignment ifelse-value aggregation-method = "average" [ ( sum elements ) / length elements ] [ reduce * elements ]

    set recruited? random-float 1 <  perceived-ea-alignment

    if recruited? [
      set color blue + 2
    ]
  ]

  set conversion-rate get-conversion-rate
end


; Function to export data as CSV for external plotting
to export-sweep-data [ data ]
  ; Create headers
  let headers (list "run_id" "n_agents" "movement_saturation" "relative_leader_influence""aggregation_method" "signal_representation""conversion_rate"
    "conversion_rate_A" "conversion_rate_B" "conversion_rate_C" "conversion_rate_D" "conversion_rate_E"
    "converted_A" "converted_B""converted_C" "converted_D" "converted_E"
    "total_A" "total_B" "total_C" "total_D" "total_E")

  ; Combine headers with data
  let csv-data fput headers data

  ; Export to CSV
  csv:to-file "sweep_data.csv" csv-data

  print "Data exported to sweep_data.csv"
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

to-report random-dirichlet [ n alpha concentration ]
  sr:set "n" n
  sr:set "c" concentration
  sr:set "a" alpha

  let vals sr:runresult "rdirichlet(n, a*c)"
  report vals
end

; Earth Mover's Distance for signal lists (exactly 5 items, values 1-5)
; Returns similarity score between 0 and 1 (1 = identical, 0 = maximally different)
to-report emd-similarity [signal1 signal2]
  ; Create frequency vectors for values 1-5
  let freq1 [0 0 0 0 0]
  let freq2 [0 0 0 0 0]

  ; Count frequencies - no validation needed since we assume valid signals
  foreach signal1 [ s ->
    set freq1 replace-item (s - 1) freq1 (item (s - 1) freq1 + 1)
  ]
  foreach signal2 [ s ->
    set freq2 replace-item (s - 1) freq2 (item (s - 1) freq2 + 1)
  ]

  ; Calculate 1D Wasserstein distance using cumulative distributions
  ; No normalization needed since both lists have exactly 5 items
  let cumsum1 0
  let cumsum2 0
  let total-distance 0

  foreach (range 5) [ i ->
    set cumsum1 cumsum1 + item i freq1
    set cumsum2 cumsum2 + item i freq2
    set total-distance total-distance + abs (cumsum1 - cumsum2)
  ]

  ; Convert distance to similarity
  ; Maximum possible distance is 20 (when [1,1,1,1,1] vs [5,5,5,5,5])
  let max-possible-distance 20
  let similarity 1 - (total-distance / max-possible-distance)

  report similarity
end
@#$#@#$#@
GRAPHICS-WINDOW
926
10
1285
370
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
-13
13
-13
13
0
0
1
ticks
30.0

SLIDER
107
10
265
43
movement-saturation
movement-saturation
0
100
41.0
1
1
%
HORIZONTAL

INPUTBOX
0
10
104
81
n-agents
50.0
1
0
Number

MONITOR
610
46
713
91
n-eas
count eas
17
1
11

PLOT
0
266
378
399
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
0
402
379
543
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
498
46
608
91
conversion rate
conversion-rate
2
1
11

SWITCH
107
47
266
80
show-links
show-links
1
1
-1000

BUTTON
0
84
267
127
Go
set sweeping? false\nsetup\ngo
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
0
579
380
792
Conversion Rate by Movement Saturation
Movement Saturation
Conversion Rate
0.0
100.0
0.0
1.0
true
true
"" ""
PENS

BUTTON
0
544
383
577
Sweep
sweep sweep-params
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
381
579
761
792
Mean Conversion Rates
Movement Saturation
Mean Conversion Rate
0.0
100.0
0.0
1.0
true
true
"" ""
PENS

CHOOSER
385
47
495
92
aggregation-method
aggregation-method
"average" "product"
0

PLOT
380
129
679
264
Number of EA signals emitted
Number of EA signals
N agents
0.0
6.0
0.0
10.0
true
false
"" ""
PENS
"5 EA signals" 1.0 1 -13345367 true "plot count turtles with [sum signals = 5]" "plotxy 5 (count turtles with [sum signals = 5])"
"4 EA signals" 1.0 1 -13791810 true "" "plotxy 4 (count turtles with [sum signals = 4])"
"3 EA signals" 1.0 1 -11221820 true "" "plotxy 3 (count turtles with [sum signals = 3])"
"2 EA signals" 1.0 1 -14835848 true "" "plotxy 2 (count turtles with [sum signals = 2])"
"1 EA signals" 1.0 1 -13840069 true "" "plotxy 1 (count turtles with [sum signals = 1])"
"0 EA signals" 1.0 1 -10899396 true "" "plotxy 0 (count turtles with [sum signals = 0])"

CHOOSER
271
47
381
92
signal-representation
signal-representation
"binary" "categorical-dirichlet"
1

CHOOSER
386
532
545
577
sweep-params
sweep-params
"full-range" "focused-population" "test-range" "custom-all"
3

SWITCH
270
94
496
127
include-movement-saturation?
include-movement-saturation?
1
1
-1000

SLIDER
268
10
494
43
relative-leader-influence
relative-leader-influence
0
100
50.0
1
1
%
HORIZONTAL

PLOT
0
129
377
263
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
496
10
713
43
recruitment-strictness
recruitment-strictness
0
100
50.0
1
1
%
HORIZONTAL

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
