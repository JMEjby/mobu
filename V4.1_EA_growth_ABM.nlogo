; to do
; conversion and desertion rate averaged over 4 time periods

; ============================================================================
; MODEL DECLARATIONS
; ============================================================================

extensions [ rnd csv ]

__includes [
  "sweep_function.nls"
  "sweep_utilities.nls"
  "sweep_headless.nls"
  "utilities.nls"
  "emd.nls"
  "funnel_layout.nls"
]

breed [ leaders leader ] ; define an agent type for leaders
breed [ eas ea ] ; define an agent type for people in the movement
breed [ outsiders outsider ] ; define an agent type for people not in the movement

; variable spaces

globals [ ; variables that are the same for the whole space
  ea-extinct?                 ; true/false if there is anyone left of EA
  conversion-rate             ; the proportion of outsiders joining EA
  desertion-rate              ; the proportion of eas leaving EA
  signal-options              ; possible signals
  cumulative-signal-sum-eas   ; cumulative sum for EAs + leaders
  cumulative-signal-sum-total ; cumulative sum for all agents
  cumulative-signal-sum-non-eas
  pop-mean                    ; the mean used for population draws
  pop-sd                      ; the sd used for population draws
  ; sweeping helpers
  sweeping?                   ; true/false variable initiating the model correctly
]

turtles-own [ ; agent-specific variables - regardless of types
  perceived-ea-alignment
  value-profile ; a letter grade reflecting EA alignment (a normally distributed trait)
  signal-weights ; for repeated signal generation
  signals ; a list of 5 binary signals that can either be EA-aligned (1) or not (0)
  time-as-type ; how long an agent has been its current type
]

eas-own [
  deserted?
  leader-elect?
]

outsiders-own [
  recruited? ; logical variable indicating whether a non-ea'er has been convinced to join the movement
]

links-own [
  perceived-alignment ; an index [0,1] estimated with signals where 0 indicates no alignment and 1 indicates complete alignment
  perceived-misalignment
  actual-alignment ;  a binary {0, 1} estimated with categories where 0 different value profiles and 1 indiciates the same value profile
]

; ============================================================================
; SETUP FUNCTIONS
; ============================================================================
to setup
  ; clear last sim - since globals are just params and sweeping? we do not need to clear those
  clear-turtles
  clear-links
  clear-patches
  reset-ticks

  setup-globals            ; set internal variables based on parameters
  setup-space              ; generate model space
  clear-all-plots
  generate-agents          ; generate agents
  generate-values          ; assign value-profiles and signals
  setup-links              ; generate alignment-based links and set link properties
  position-agents-funnel   ; position agents based on perceived-ea-alignment (funnel layout)

  if not sweeping? [ update-plots ]
end

to setup-globals
  set ea-extinct? false
  set semester 1

  ; set population values distribution parameters
  set pop-mean item ( position population-composition [ "normal-narrow" "normal-wide"] ) [ 0 0 ]
  set pop-sd item ( position population-composition [ "normal-narrow" "normal-wide"] ) [ 1 1.5 ]

  ; set signal-options
  set signal-options ifelse-value signal-representation = "binary" [ ( list 0 1 ) ] [ ( list 6 5 4 3 2 1 ) ]

  ; reset cum sums
  set cumulative-signal-sum-eas 0
  set cumulative-signal-sum-total 0
  set cumulative-signal-sum-non-eas 0
end

; function to label and size the world appropriately for funnel layout
to setup-space
  ; Set world size for funnel: y-axis from -10 to 10, x-axis wide enough for all agents
  let world-x-size ceiling (sqrt n-agents * 3) ; provide enough x-space for jittering
  resize-world ( - world-x-size ) world-x-size -10 10

  ; white background
  ask patches [ set pcolor 9.9 ]

  ; add y-coordinate labels on leftmost patches
  ask patches with [pxcor <= min-pxcor + 6 ] [
    set plabel-color black
    ifelse pycor != max-pycor [
      if pxcor = min-pxcor [ set plabel "[" ]
      if pxcor = min-pxcor + 2 [ set plabel ( pycor + 10 ) / 20 ]
      if pxcor = min-pxcor + 3 [ set plabel  ","]
      if pxcor = min-pxcor + 5 [ set plabel ( pycor + 11 ) / 20 ]
      if pxcor = min-pxcor + 6 [ set plabel  "[" ]
    ][
      if pxcor = min-pxcor [ set plabel "[" ]
      if pxcor = min-pxcor + 3 [ set plabel  ( pycor + 10 ) / 20 ]
      if pxcor = min-pxcor + 6 [ set plabel  "]" ]
    ]
  ]
end

; function generating agents
to generate-agents
  ; number of ea agents is a proportion of the population corresponding to movement saturation. since the leader is generated separately, we subtract 1.
  let n-eas ceiling ( ( n-agents * movement-saturation / 100 ) ) - 1
  ; number of outsiders is the remainder
  let n-outsiders n-agents - n-eas - 1


  ; create EA agents at origin for now
  create-ordered-eas ( n-eas ) [
    setxy 0 0 ; temporary position
    set shape "triangle" ; make EA agents traingles
    set deserted? false
    set leader-elect? false
    set time-as-type 1
  ]

  ; create one EA-leader at origin for now
  create-leaders 1 [
    setxy 0 0 ; temporary position
    set shape "star" ; make EA leaders stars
    set time-as-type 1
  ]

  ; create people outside the movement at origin for now
  create-ordered-outsiders n-outsiders [
    setxy 0 0 ; temporary position
    set shape "circle" ; make outsiders circles
    set recruited? false
    set time-as-type 1
  ]

  ask turtles [
    create-links-with other turtles ; connect everyone for alignment estimation
  ]
end

to generate-values
  ; generate values from the relevant population distribution
  let population-values generate-population-values

  ; Create list of all available indices
  let population-indices n-values length population-values [ i -> i ]

  ; assign leaders value-profile
  ask leaders [
    ifelse leader-election = "external" [
      set value-profile external-leader-value-profile
    ] [
      ; determine trait value depending on leader and population parameters
      let selected-index leader-index-selection population-indices population-values

      ; assign trait values either the index or using the failsafe for deterministic values
      ifelse is-number? selected-index [
        set value-profile item selected-index population-values
        set value-profile value-category value-profile ; convert trait value drawn into value profile
      ][
        set value-profile selected-index
      ]

      ; Remove this index from available pool
      set population-indices remove ( ifelse-value is-number? selected-index [ selected-index ] [ ifelse-value leader-values = "value-profile-D" [ length population-indices ] [ 0 ] ] ) population-indices
    ]
  ]

  ; assign eas value-profiles
  ask eas [
    ; EAs have moderate bias toward high values
    let selected-index select-weighted-index ea-initial-value-bias population-indices population-values ; moderate bias
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
  ]

  ; assign signal-weights and generate signals
  ask turtles [ setup-signals ]

  ; make plot of signal weights
  if not sweeping? [
    setup-signal-probability-plot "EA"
    setup-signal-probability-plot "Non-EA"
    setup-mean-signal-plot
  ]
end

to-report generate-population-values
  ; Generate normally distributed values for entire population
  let population-values [ ]

  repeat ifelse-value leader-election = "external" [ n-agents - 1 ] [ n-agents ] [
    set population-values lput ( random-normal pop-mean pop-sd ) population-values
  ]
  ; Sort values in descending order (highest first)
  set population-values sort-by > population-values

  report population-values
end

to-report leader-index-selection [ population-indices population-values ]
  ; Default: random-uniform
  let selected-index one-of population-indices

  ; Draw one A value
  if leader-values = "value-profile-A" [
    ; leader picks one A value
    let A-indices filter [ i -> ( item i population-values ) > 1.5 ] population-indices

    ifelse length A-indices > 0 [
      set selected-index one-of A-indices
    ] [
      set selected-index "A"
      print "Warning: The population value draw did not contain the values needed to assign the leader value profile A. They were given value profile A exceptionally and the most EA-aligned value drawn was exchanged."
    ]
  ]

  ; Draw one B value
  if leader-values = "value-profile-B" [
    ; leader picks one B value
    let B-indices filter [ i -> ( item i population-values ) <= 1.5 and ( item i population-values ) > 0.5 ] population-indices

    ifelse length B-indices > 0 [
      set selected-index one-of B-indices
    ] [
      set selected-index "B"
      print "Warning: The population value draw did not contain the values needed to assign the leader value profile B. They were given value profile B exceptionally and the most EA-aligned value drawn was exchanged."
    ]
  ]

   ; Draw one C value
  if leader-values = "value-profile-C" [
    ; leader picks one C value
    let C-indices filter [ i -> ( item i population-values ) <= 0.5 and ( item i population-values ) > -0.5 ] population-indices

    set selected-index one-of C-indices
  ]

   ; Draw one D value
  if leader-values = "value-profile-D" [
    ; leader picks one D value
    let D-indices filter [ i -> ( item i population-values ) <= -0.5 and ( item i population-values ) > -1.5 ] population-indices

    ifelse length D-indices > 0 [
      set selected-index one-of D-indices
    ] [
      set selected-index "D"
      print "Warning: The population value draw did not contain the values needed to assign the leader value profile D. They were given value profile D exceptionally and the least EA-aligned value drawn was exchanged."
    ]
  ]

  report selected-index
end

to setup-signals
  set color item (position value-profile ["A" "B" "C" "D" "E"]) [105 107 115 137 135] ; set colour to correspond to value profile

  ifelse signal-representation = "categorical" [
    set signal-weights item ( position value-profile [ "A" "B" "C" "D" "E" ] ) ( list
      [ 0.5 0.3 0.15 0.0399 0.01 0.0001 ]
      [ 0.01 0.4 0.4 0.15 0.0395 0.0005 ]
      [ 0.01 0.02 0.12 0.7 0.12 0.03 ]
      [ 0.0005 0.0395 0.15 0.2 0.4 0.21 ]
      [ 0.0001 0.005 0.005 0.0399 0.55 0.4 ]
    )

    ; generate signals probabilistically from value profile
    set signals generate-signals n-signals
  ] [
    set signal-weights item ( position value-profile [ "A" "B" "C" "D" "E" ] ) ( list [ 0.1 0.9 ] [ 0.3 0.7 ] [ 0.5 0.5 ] [ 0.7 0.3 ] [ 0.9 0.1 ] )

    ; generate signals probabilistically from value profile
    set signals generate-signals n-signals
  ]
end

to setup-links
  ask links [
    infer-alignment ; make signal-based inference of alignment
    hide-link
  ]

  ; compute initial perceived-ea-alignment for all agents
  ask eas [ compute-ea-alignment ]
  ask outsiders [ compute-ea-alignment ]
  ask leaders [ set perceived-ea-alignment 1.0 ] ; leaders have perfect alignment
end

; ============================================================================
; MAIN SIMULATION LOOP
; ============================================================================
to go
  re-assign-groups        ; convert recruited outsiders to EAs and deserted EAs to outsiders
  check-ea-extinction
  if ea-extinct? [ stop ]
  re-elect-leader         ; determine new EA leader if needed
  update-links            ; alignment inference and link visuals
  update-ea-values        ; update EAs values according to signal representation
  recruit-eaers           ; outsiders determine if they want to join EA
  desert-eaers            ; EAs determine if they want to leave EA
  update-signals          ; generate new signals
  position-agents-funnel  ; re position based on updated alignment inference
  tick                    ; increment time
end

; ============================================================================
; ALIGNMENT INFERENCE FUNCTIONS
; ============================================================================
to update-links
  ask links [
    infer-alignment
    set thickness perceived-alignment
    hide-link
  ]
end

to infer-alignment
  let signal1 [signals] of end1
  let signal2 [signals] of end2

  ifelse signal-representation = "categorical" [
    set perceived-misalignment emd-distance signal1 signal2 "dir" ; get emd directional adjustments
    set perceived-alignment emd-distance signal1 signal2 "sim" ; get emd similarity
  ] [
    set perceived-misalignment ( sum signal2  - sum signal1 ) / length ( signal1 )
    set perceived-alignment 1 - ( abs ( perceived-misalignment ) )
  ]

  set actual-alignment ifelse-value ( [ value-profile ] of end1 = [ value-profile ] of end2 ) [ 1 ] [ 0 ]
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

  set perceived-ea-alignment mean elements
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
  let perceived-ea-misalignment mean elements

  report perceived-ea-misalignment
end

to-report compute-ea-misalignment-cat
  let ea-leader-misalignment-vector n-values (length signal-options) [ 0 ]
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
  ; Combine weighted components element-wise (addition for average)
  let perceived-ea-misalignment-vector  ( map [ [a b] -> ( a + b ) ] weighted-leader-misalign weighted-avg-misalign )

  report perceived-ea-misalignment-vector
end

to update-ea-values
  ifelse signal-representation = "categorical"
  [ update-ea-values-cat ]
  [ update-ea-values-binary]
end

to update-ea-values-binary ;; ONLY WORKS ON BINARY SIGNAL REPRESENTATIONS
  ask eas  [
    let update compute-ea-misalignment-binary

    ; compute updated weights
    let w1 item 1 signal-weights
    let log-odds  ( ln ( w1 / ( 1 - w1 ) ) ) + update * social-influence
    let w1-updated exp log-odds / ( 1 + exp log-odds )
    let w0-updated 1 - w1-updated

    ; update weights
    set signal-weights list w0-updated w1-updated
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

; ============================================================================
; AGENT BEHAVIOR FUNCTIONS
; ============================================================================
to recruit-eaers
  ask outsiders [
    compute-ea-alignment
    set recruited? random-float 1 <  ( perceived-ea-alignment ^ ( 100 ^ log-recruitment-strictness ) )
    if recruited? [
      set size .5
    ]
  ]

  set conversion-rate get-conversion-rate
end

to desert-eaers
  ask eas with [
    compute-ea-alignment
    set deserted? random-float 1 < ( 1 - ( perceived-ea-alignment ^ ( 100 ^ ( log-desertion-strictness * time-as-type ) ) ) )
    if deserted? [
      set size .5
    ]
  ]
  set desertion-rate get-desertion-rate
end

to re-assign-groups
  ask turtles [
    set time-as-type time-as-type + 1
  ]

  convert-recruited-to-eas
  convert-deserted-to-outsiders
end

to update-signals
  update-cumulative-signal-sums
  if not sweeping? [
    update-signal-probability-plot "EA"
    update-signal-probability-plot "Non-EA"
    update-mean-signal-plot
  ]
  ask turtles [
    if random-float 1 < ( new-signal-rate / 100 ) [
      set signals lput ( first generate-signals 1 ) but-first signals
    ]
  ]
end

to convert-recruited-to-eas
  ; Convert recruited outsiders to eas
  ask outsiders with [recruited? = true] [
    ; Store current properties
    let my-xcor xcor
    let my-ycor ycor
    let my-color color
    let my-value-profile value-profile
    let my-signals signals
    let my-perceived-ea-alignment perceived-ea-alignment

    ; Create new ea agent
    hatch-eas 1 [
      set xcor my-xcor
      set ycor my-ycor
      set color my-color
      set value-profile my-value-profile
      set signals my-signals
      set perceived-ea-alignment my-perceived-ea-alignment
      set time-as-type 1
      set deserted? false  ; initialize as not deserted
      set shape "triangle"   ; set ea shape
      set size 1

      ; Create undirected links with all other agents
      create-links-with other turtles
    ]
    die
  ]
end

to convert-deserted-to-outsiders
; Convert deserted eas to outsiders
  ask eas with [deserted? = true] [
    ; Store current properties
    let my-xcor xcor
    let my-ycor ycor
    let my-color color
    let my-value-profile value-profile
    let my-signals signals
    let my-perceived-ea-alignment perceived-ea-alignment

    ; Create new outsider agent
    hatch-outsiders 1 [
      set xcor my-xcor
      set ycor my-ycor
      set color my-color
      set value-profile my-value-profile
      set signals my-signals
      set perceived-ea-alignment my-perceived-ea-alignment
      set time-as-type 1
      set recruited? false  ; initialize as not recruited
      set shape "circle"  ; set outsider shape
      set size 1

      ; Create undirected links with all other agents
      create-links-with other turtles
    ]
    die
  ]
end

; ============================================================================
; STUDENT AND LEADER LIFECYCLE FUNCTIONS
; ============================================================================
to check-ea-extinction
  ; Check if EAs and leaders are extinct AND leader is not external
  ; External leaders can be reinstantiated, so extinction only matters for internal leadership
  ifelse count eas = 0 and count leaders = 0 and leader-election != "external" [
    set ea-extinct? true

    if not sweeping? [
      ; Interactive mode: show message
      user-message "The EA student society has died out! All EAs and leaders have left or graduated, and leadership is not externally sustained. Simulation ended."
    ]
  ][
    ; If we reach here, EAs still exist OR external leadership can sustain the movement
    set ea-extinct? false
  ]
end

to re-elect-leader
  if leader-election = "external" [
    ask leaders with [ time-as-type > leader-lifespan ] [ die ]
  ]

  if not ( any? leaders ) [
    elect-new-leader
    setup-new-leader
  ]
end

to elect-new-leader
  ( ifelse
    leader-election = "external" [
      create-external-leader
    ] leader-election = "most-aligned" [
      ask one-of eas with [ perceived-ea-alignment = max [ perceived-ea-alignment ] of eas ] [
        set leader-elect? true
      ]
    ] [
      ask one-of eas [set leader-elect? true]
    ]
  )
end

to setup-new-leader
  if leader-election != "external" [
    convert-ea-to-leader
  ]

  ask leaders [
    set shape "star"
    set perceived-ea-alignment 1
    set ycor 10
    set xcor 0
    set time-as-type 1
  ]
end

to create-external-leader
  create-leaders 1 [
    set value-profile external-leader-value-profile
    set year 1
    setup-signals
    create-links-with other turtles
  ]
end

to convert-ea-to-leader
 ; Convert elected ea to leader
  ask eas with [leader-elect? = true] [
    ; Store current properties
    let my-color color
    let my-value-profile value-profile
    let my-signals signals
    let my-year year
    let my-perceived-ea-alignment perceived-ea-alignment

    ; Create new outsider agent
    hatch-leaders 1 [
      set color my-color
      set value-profile my-value-profile
      set signals my-signals
      set year my-year
      set perceived-ea-alignment my-perceived-ea-alignment

      ; Create undirected links with all other agents
      create-links-with other turtles
    ]
    die
  ]
end
@#$#@#$#@
GRAPHICS-WINDOW
932
133
1733
415
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
-30
30
-10
10
1
1
1
ticks
30.0

SLIDER
291
46
502
79
movement-saturation
movement-saturation
0
50
5.0
1
1
%
HORIZONTAL

INPUTBOX
0
10
84
78
n-agents
100.0
1
0
Number

MONITOR
379
329
438
374
n-eas
count eas
17
1
11

PLOT
1225
417
1513
579
EA Pop: Year 1 Value Predispositions
Intital Value Profiles E-A
N agents
1.0
6.0
0.0
5.0
true
false
"" ""
PENS
"EAs" 1.0 1 -13345367 true "clear-plot\nplotxy 5 (count eas with [value-profile = \"A\"])\nplotxy 4 (count eas with [value-profile = \"B\"])  \nplotxy 3 (count eas with [value-profile = \"C\"])\nplotxy 2 (count eas with [value-profile = \"D\"])\nplotxy 1 (count eas with [value-profile = \"E\"])\nplotxy 5 (count eas with [value-profile = \"E\"])" "clear-plot\nplotxy 5 (count eas with [value-profile = \"A\"])\nplotxy 4 (count eas with [value-profile = \"B\"])  \nplotxy 3 (count eas with [value-profile = \"C\"])\nplotxy 2 (count eas with [value-profile = \"D\"])\nplotxy 1 (count eas with [value-profile = \"E\"])\nplotxy 5 (count eas with [value-profile = \"E\"])\nplotxy 5 (count eas with [value-profile = \"E\"])\nplotxy 5 (count eas with [value-profile = \"E\"])"

PLOT
1516
417
1803
579
Non-EA value profile distribution
Value Profiles E-A
N agents
1.0
6.0
0.0
5.0
true
false
"" ""
PENS
"default" 1.0 1 -13840069 true "clear-plot\nplotxy 5 (count outsiders with [value-profile = \"A\"])\nplotxy 4 (count outsiders with [value-profile = \"B\"])  \nplotxy 3 (count outsiders with [value-profile = \"C\"])\nplotxy 2 (count outsiders with [value-profile = \"D\"])\nplotxy 1 (count outsiders with [value-profile = \"E\"])" "clear-plot\nplotxy 5 (count outsiders with [value-profile = \"A\"])\nplotxy 4 (count outsiders with [value-profile = \"B\"])  \nplotxy 3 (count outsiders with [value-profile = \"C\"])\nplotxy 2 (count outsiders with [value-profile = \"D\"])\nplotxy 1 (count outsiders with [value-profile = \"E\"])"

BUTTON
222
83
288
127
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

CHOOSER
291
83
442
128
signal-representation
signal-representation
"binary" "categorical"
1

SLIDER
505
46
744
79
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
932
417
1221
578
Population value profile distribution
Value Profiles E-A
N agents
1.0
6.0
0.0
5.0
true
false
"" ""
PENS
"default" 1.0 1 -11221820 true "clear-plot\nplotxy 5 (count turtles with [value-profile = \"A\"])\nplotxy 4 (count turtles with [value-profile = \"B\"])  \nplotxy 3 (count turtles with [value-profile = \"C\"])\nplotxy 2 (count turtles with [value-profile = \"D\"])\nplotxy 1 (count turtles with [value-profile = \"E\"])" "clear-plot\nplotxy 5 (count turtles with [value-profile = \"A\"])\nplotxy 4 (count turtles with [value-profile = \"B\"])  \nplotxy 3 (count turtles with [value-profile = \"C\"])\nplotxy 2 (count turtles with [value-profile = \"D\"])\nplotxy 1 (count turtles with [value-profile = \"E\"])"

SLIDER
694
10
927
43
log-recruitment-strictness
log-recruitment-strictness
-1
1
0.4
0.1
1
NIL
HORIZONTAL

SLIDER
458
10
691
43
log-desertion-strictness
log-desertion-strictness
-1
1
-0.4
0.1
1
NIL
HORIZONTAL

BUTTON
0
82
67
127
setup
set sweeping? false\nsetup
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
134
82
219
127
Go once
go
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
70
83
131
127
Go
go
T
1
T
OBSERVER
NIL
G
NIL
NIL
0

PLOT
0
329
376
514
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

PLOT
444
132
927
327
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

SLIDER
746
46
927
79
new-signal-rate
new-signal-rate
0
100
100.0
5
1
%
HORIZONTAL

PLOT
0
132
438
326
Mean time-as-type
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

SLIDER
290
10
454
43
social-influence
social-influence
0
1
0.5
0.05
1
NIL
HORIZONTAL

PLOT
444
331
927
517
EA Values over Time
Time
Signal Probability
0.0
10.0
0.0
1.0
true
true
"" ""
PENS

TEXTBOX
940
98
1019
132
Perceived EA Alignment
13
0.0
1

INPUTBOX
88
10
165
78
n-signals
5.0
1
0
Number

PLOT
446
730
926
919
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

PLOT
444
521
927
723
Non-EA Values over Time
Time
Signal probability
0.0
10.0
0.0
1.0
true
true
"" ""
PENS

CHOOSER
444
83
562
128
leader-values
leader-values
"value-profile-A" "value-profile-B" "value-profile-C" "value-profile-D"
0

CHOOSER
564
83
664
128
leader-election
leader-election
"random-ea" "most-aligned" "external"
1

PLOT
0
516
439
722
Average Signal
Time
Signal Mean
0.0
10.0
-2.0
3.0
true
true
"clear-plot\n\n; Set y-axis range based on signal representation\n  ifelse signal-representation = \"categorical\" [\n    set-plot-y-range -2 3\n  ] [\n    set-plot-y-range 0 1\n  ]" ""
PENS
"Leader" 1.0 0 -14730904 true "" ""
"EAs" 1.0 0 -13345367 true "" ""
"Non-EAs" 1.0 0 -2064490 true "" ""

INPUTBOX
168
10
287
78
leader-lifespan
4.0
1
0
Number

MONITOR
820
203
926
248
Desertion Rate (%)
desertion-rate * 100
2
1
11

MONITOR
817
250
927
295
Conversion rate (%)
conversion-rate * 100
2
1
11

CHOOSER
777
83
928
128
population-composition
population-composition
"normal-narrow" "normal-wide" "ea-skew" "non-ea-skew"
0

TEXTBOX
1040
10
1563
119
Shape → Type           Colour → Profile         Size → Recruitment/Desertion\nStar       = Leader       Dark blue  = A            Small = Type changing\nTriangle = EA             Light blue  = B            Large = Type unchanged\nCircle     = Non‑EA      Purple       = C\n                                 Light pink = D\n                                 Dark pink  = E
13
0.0
1

PLOT
0
728
439
918
Cumulative Signal Sum
Time
Cumulative Sum
0.0
10.0
0.0
10.0
true
true
"clear-plot\n\nset-plot-y-range ( n-agents * ifelse-value signal-representation = \"bianry\" [0] [-2]) ( n-agents * ifelse-value signal-representation = \"binary\" [ 1 ] [ 3 ])" ""
PENS
"EA" 1.0 0 -13345367 true "" "plot cumulative-signal-sum-eas"
"Total" 1.0 0 -16777216 true "" "plot cumulative-signal-sum-total"

CHOOSER
666
83
775
128
ea-initial-value-bias
ea-initial-value-bias
0 0.5 1.5
2

MONITOR
932
10
996
55
semester
semester
17
1
11

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
