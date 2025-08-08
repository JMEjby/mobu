extensions [sr rnd csv ]

breed [ leaders leader ] ; define an agent type for leaders
breed [ eas ea] ; define an agent type for people in the movement
breed [ outsiders outsider ] ; define an agent type for people not in the movement

; variable spaces

globals [
  conversion-rate ; the proportion of outsiders joining EA
  ; sweeping helpers
  m-sat-values ; value range for movement-saturation
  n-agent-values ; value range for n-agents
  sweeping? ; true/false variable initiating the model correctly
] ; variables that are the same for the whole space

turtles-own [ ; agent-specific variables - regardless of types
  value-profile ; a letter grade reflecting EA alignment (a normally distributed trait)
  signals ; a list of 5 binary signals that can either be EA-aligned (1) or not (0)
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
  perceived-ea-similarity
]

links-own [
  perceived-similarity ; an index [0,1] estimated with signals where -1 indicates no similarity and 1 indicates complete similarity
  actual-similarity ;  a binary {0, 1} estimated with categories where -1 different value profiles and 1 indiciates the same value profile
]

; define model initiation
; 1. spawn agents of all types and define space
; 2. assign agent value category dependent on their type
; 3. generate initial agent signals from value category
; 4. initial similarity estimation from signals
to setup
  ifelse sweeping? [ ask turtles [ die ] ask links [ die ] ] [ clear-all ] ; clear last sim. If we are sweeping parameters we only clear the turtles and links
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

  ; generate similarity-based links and set link properties
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
  ; generate agents and position in space
  generate-agents n-eas n-outsiders outsider-radius ea-radius

  generate-values ; assign value profiles

  ask turtles [
    generate-signals ; generate signals probabilistically from value profile
    create-links-with other turtles ; connect everyone for similarity estimation
  ]
end

to setup-links
  ask links [
    infer-similarity ; make signal-based inference of similarity
    set thickness perceived-similarity
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

; function that creates a set of binary signals dependent on value profile
to generate-signals
  let weights item ( position value-profile [ "A" "B" "C" "D" "E" ] ) [ 0.9 0.7 0.5 0.3 0.1 ]
  set weights list ( 1 - weights ) weights
  set signals n-values 5 [ weighted-prob-draw weights [ 0 1 ] ]
end

to infer-similarity
  let my-signals [ signals ] of both-ends
  set perceived-similarity 1 - abs ( sum ( item 0 my-signals ) - sum ( item 1 my-signals ) ) / length ( item 0 my-signals )
  ;set perceived-similarity ( perceived-similarity - 0.5 ) * 2
  set actual-similarity ifelse-value ( [ value-profile ] of end1 = [ value-profile ] of end2 ) [ 1 ] [ 0 ]
end

to go
  recruit-eaers
  tick
  update-plots
end

to sweep
  ; Check with user before starting the sweep
  let proceed? user-yes-or-no? (word
    "Start parameter sweep with the following settings?\n\n"
    "Movement saturation: 5% to 95% (step: 5%)\n"
    "Population sizes: [25 50 100 250 500 1000]\n"
    "Runs per combination: 10\n"
    "Total simulations: " (length (range 5 96 5) * length [25 50 100 250 500 1000] * 5) "\n\n"
    "This may take several minutes. Continue?")

  ; Exit if user chooses not to proceed
  if not proceed? [
    user-message "Sweep cancelled."
    stop
  ]

  ; Initialize sweep by clearing everything and setting up parameter ranges
  clear-all
  set sweeping? true

  set m-sat-values ( range 5 96 5 )  ; Movement saturation from 5% to 95% in steps of 5
  set n-agent-values [ 25 50 ]  ; Different population sizes to test

  ; Clear any existing plot data to start fresh
  clear-all-plots

  ; Define distinct colors for each population size
  let pen-colors [ red blue green orange violet cyan ]
  let color-index 0

  ; Initialize data collection lists for R plotting
  let all-data []  ; Will store: [n-agents movement-saturation conversion-rate run-id]

  ; Loop through each population size
  foreach n-agent-values [ n ->
    ; Create a pen for individual simulation runs (as lines)
    set-current-plot "Conversion Rate by Movement Saturation"
    create-temporary-plot-pen (word "n=" n)
    set-plot-pen-color item color-index pen-colors  ; Use predefined colors

    ; Create a pen for means in the separate means plot (as lines)
    set-current-plot "Mean Conversion Rates"
    create-temporary-plot-pen (word "n=" n)
    set-plot-pen-color item color-index pen-colors  ; Same color as individual runs

    ; Move to next color for next population size
    set color-index color-index + 1

    ; Loop through each movement saturation level
    foreach m-sat-values [m ->
      ; Set parameters for this combination
      set n-agents n
      set movement-saturation m
      let cr-values []  ; List to store conversion rates from multiple runs

      ; Switch to individual runs pen for plotting
      set-current-plot "Conversion Rate by Movement Saturation"
      set-current-plot-pen (word "n=" n)

      ; Run the simulation 5 times with the same parameters
      let run-id 0
      repeat 10 [
        set run-id run-id + 1
        setup  ; Initialize model with current parameters
        go     ; Run one simulation step

        plotxy m conversion-rate  ; Plot individual result
        set cr-values lput conversion-rate cr-values  ; Add to list for mean calculation

        ; Store data for R plotting: [n-agents movement-saturation conversion-rate run-id]
        set all-data lput (list n m conversion-rate run-id) all-data
      ]

      ; Calculate mean of the 5 runs and plot in the means plot
      let mean-cr mean cr-values
      set-current-plot "Mean Conversion Rates"
      set-current-plot-pen (word "n=" n)
      plotxy m mean-cr
    ]

    ; Progress indicator for this population size
    print (word "Completed n-agents = " n)
  ]

  ; Reset sweep mode
  set sweeping? false

  ; Export data to CSV for external plotting
  ifelse export-sweep-plot? [
    create-r-plot all-data
    user-message "Sweep complete! All parameter combinations have been tested. R plot saved as 'sweep_results.png'."
  ] [
    export-sweep-data all-data
    user-message "Sweep complete! All parameter combinations have been tested. Data exported to 'sweep_data.csv'."
  ]
end

to recruit-eaers
  ask outsiders [
    let ea-leader-similarity first [ perceived-similarity ] of my-links with [ member? other-end leaders ]
    let avg-ea-similarity mean [perceived-similarity] of my-links with [ member? other-end eas]
    set perceived-ea-similarity ea-leader-similarity ;* avg-ea-similarity * ( movement-saturation / 100 )
    set recruited? random-float 1 <  perceived-ea-similarity

    if recruited? [
      set color blue + 2
    ]
  ]

  set conversion-rate get-conversion-rate
end

; Function to create the R visualization using Simple R extension
to create-r-plot [ data ]
  ; Setup R session (replaces r:clear)
  sr:setup

  ; Send data to R using Simple R extension
  let n-agents-list (map first data)
  let movement-sat-list (map [row -> item 1 row] data)
  let conversion-rate-list (map [row -> item 2 row] data)
  let run-id-list (map [row -> item 3 row] data)

  sr:set "n_agents" n-agents-list
  sr:set "movement_sat" movement-sat-list
  sr:set "conversion_rate" conversion-rate-list
  sr:set "run_id" run-id-list

  ; Load libraries and create data frame
  (sr:run
    "library(ggplot2)"
    "library(RColorBrewer)"
    "library(dplyr)"
  )

  (sr:run
    "df <- data.frame(n_agents = n_agents, movement_saturation = movement_sat, conversion_rate = conversion_rate, run_id = run_id)"
    "df$n_agents <- factor(df$n_agents)"
  )

  ; Calculate summary statistics with ribbon bounds
  (sr:run
    "summary_df <- df %>%"
    "  group_by(n_agents, movement_saturation) %>%"
    "  summarise(mean_cr = mean(conversion_rate),"
    "            sd_cr = sd(conversion_rate),"
    "            .groups = 'drop') %>%"
    "  mutate(ribbon_upper = ifelse(mean_cr + sd_cr > 1, 1, mean_cr + sd_cr),"
    "         ribbon_lower = ifelse(mean_cr - sd_cr < 0, 0, mean_cr - sd_cr))"
  )

  ; Create custom facet labels function
  sr:run "facet_labels <- function(x) { paste(x, 'agents') }"

  ; Create and save the plot in one command
  (sr:run
    "p <- ggplot() +"
    "  geom_line(data = df, aes(x = movement_saturation, y = conversion_rate,"
    "                          color = n_agents,"
    "                          group = interaction(n_agents, run_id)),"
    "            alpha = 0.6) +"
    "  geom_ribbon(data = summary_df,"
    "              aes(x = movement_saturation,"
    "                  ymin = ribbon_lower,"
    "                  ymax = ribbon_upper,"
    "                  fill = n_agents),"
    "              alpha = 0.4) +"
    "  geom_line(data = summary_df,"
    "            aes(x = movement_saturation, y = mean_cr, color = n_agents),"
    "            size = 1) +"
    "  geom_point(data = summary_df,"
    "             aes(x = movement_saturation, y = mean_cr, color = n_agents),"
    "             size = 1.8) +"
    "  facet_wrap(~ n_agents, labeller = labeller(n_agents = facet_labels), axes = 'all', ncol = 2) +"
    "  scale_x_continuous(breaks = c(0, 25, 50, 75, 100), limits = c(0, 100)) +"
    "  scale_y_continuous(breaks = seq(0, 1, 0.2), limits = c(0, 1)) +"
    "  scale_color_brewer(type = 'qual', palette = 'Set2') +"
    "  scale_fill_brewer(type = 'qual', palette = 'Set2') +"
    "  labs(x = 'Movement Saturation (%)',"
    "       y = 'Conversion Rate',"
    "       title = 'Conversion Rate by Movement Saturation and Population Size') +"
    "  theme(panel.background = element_rect(fill = 'white', color = 'black'),"
    "        panel.grid.major.y = element_line(color = 'grey'),"
    "        panel.grid.minor = element_line(color = NA),"
    "        panel.grid.major.x = element_line(color = NA),"
    "        strip.background = element_rect(fill = 'white', color = 'black'),"
    "        axis.line = element_line(color = 'black'),"
    "        axis.ticks = element_line(color = 'black'),"
    "        legend.position = 'none',"
    "        legend.background = element_rect(fill = 'white', color = 'black'),"
    "        axis.text = element_text(color = 'black', size = 14),"
    "        axis.title = element_text(face = 'bold', size = 16),"
    "        title = element_text(size = 16),"
    "        strip.text = element_text(size = 14, face = 'bold'))"
    ""
    "ggsave('sweep_results.png', p, width = 17, height = 21, dpi = 800, units = 'cm')"
  )

  print "R plot saved as sweep_results.png"
end

; Function to export data as CSV for external plotting
to export-sweep-data [ data ]
  ; Create headers
  let headers (list "n_agents" "movement_saturation" "conversion_rate" "run_id")

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
@#$#@#$#@
GRAPHICS-WINDOW
765
10
1124
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
14.0
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
383
10
495
55
n-eas
count eas
17
1
11

PLOT
0
260
378
393
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
123
377
257
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

PLOT
0
396
379
537
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
385
72
495
117
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
117
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
596
577
Sweep
sweep
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

SWITCH
598
544
762
577
export-sweep-plot?
export-sweep-plot?
1
1
-1000

PLOT
380
123
758
258
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

PLOT
381
261
758
537
Mean number of EA signals by agent type
Agent type 
Number of  EA signal
1.0
4.0
0.0
5.0
true
true
"" ""
PENS
"Leader" 1.0 1 -14730904 true "plotxy 1 ( mean [sum signals] of leaders )" "plotxy 1 ( mean [sum signals] of leaders )"
"EAs" 1.0 1 -13345367 true "plotxy 2 ( mean [sum signals] of eas )" "plotxy 2 ( mean [sum signals] of eas )"
"Non-EAs" 1.0 1 -2064490 true "plotxy 3 ( mean [sum signals] of outsiders )" "plotxy 3 ( mean [sum signals] of outsiders )"

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
