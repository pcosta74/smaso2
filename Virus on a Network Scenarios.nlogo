directed-link-breed [dlinks dlink]
undirected-link-breed [ulinks ulink]


globals
[
  infected-color
  resistant-color
  susceptible-color
  normal-edge-color
  disabled-edge-color
  quarantined-edge-color

  mutation?
  hatch-timer
  known-viruses
  available-vaccines
]

turtles-own
[
  infected?           ;; if true, the turtle is infectious
  infected-with       ;; the virus string infecting the turle
  vaccines            ;; the vaccination list
  resistant-to        ;; the resistance list
  quarantined?        ;; if true, the turtle is quanrantined
  virus-check-timer   ;; number of ticks since this turtle's last virus-check
  vaccine-check-timer  ;; number of ticks since this turtle's last immunization
  time-to-live        ;; number of ticks for this turtle to die
  decay-rate          ;; number of ticks to remove from ttl by tick
]

to setup
  clear-all
  setup-globals
  setup-nodes
  reset
  setup-spatially-clustered-network

end

to setup-globals
  set infected-color red
  set resistant-color gray
  set susceptible-color green

  set normal-edge-color white
  set disabled-edge-color gray - 2
  set quarantined-edge-color gray - 4

  set mutation? false
  set hatch-timer average-turtle-birthrate
  set known-viruses ( list spawn-new-virus )
  set available-vaccines known-viruses
end

to setup-nodes
  create-turtles number-of-nodes [ spawn-node ]
end

to setup-spatially-clustered-network
  let num-links (average-node-degree * count turtles) / 2
  while [count links < num-links ] [ spawn-links ]
  ; make the network look a little prettier
  repeat 10 [layout-spring turtles links 0.3 (world-width / (sqrt number-of-nodes)) 1]
end

to spawn-node
  ; for visual reasons, we don't put any nodes *too* close to the edges
  setxy (random-xcor * 0.95) (random-ycor * 0.95)
  set shape "person"
  set size 2.25

  set infected? false
  set infected-with ""
  set vaccines []
  set resistant-to []
  set quarantined? false
  set virus-check-timer random virus-check-frequency
  set vaccine-check-timer vaccination-check-frequency ;random vaccination-check-frequency

  ; set time to live over medium life-expectancy
  set time-to-live ( ifelse-value (random 100 < 50)
      [average-turtle-lifespan + precision (random (average-turtle-lifespan / 2)) 0]
      [average-turtle-lifespan - precision (random (average-turtle-lifespan / 2)) 0] )
  set decay-rate 1

  become-susceptible item 0 known-viruses
end

to spawn-links
  ask one-of turtles
  [
    let choice (min-one-of (other turtles with [not link-neighbor? myself])
      [distance myself])
    if choice != nobody [
      let link-color normal-edge-color
      ; TO DO: set proper link color

      ifelse directed-links?
      [ create-dlink-to choice [set color link-color] ]
      [ create-ulink-with choice [set color link-color] ]
    ]
  ]
end

to reset
  let virus-string (item 0 known-viruses)

  ask turtles [
    become-susceptible virus-string
  ]
  ask n-of initial-vaccination-size turtles
    [ do-vaccination virus-string ]
  ask n-of initial-resistance-size turtles
    [ become-resistant virus-string ]
  ask n-of initial-outbreak-size turtles
    [ become-infected virus-string ]
  ask links [ set color normal-edge-color ]
  reset-ticks
end

; TODO: review
to reset-default-configs
  ; reset default values to original model's
  set number-of-nodes 150
  set average-node-degree 6
  set initial-outbreak-size 3
  set virus-spread-chance 2.5
  set virus-check-frequency 1
  set recovery-chance 5.0
  set gain-resistance-chance 5

  ; set new globals' to remove meddling with original model
  set directed-links? false
  set dynamic-network? false
  set initial-resistance-size 0
  set initial-vaccination-size 0
  set vaccination-chance 0
  set vaccine-effectiveness 0
  set vaccination-check-frequency 1
  set average-turtle-lifespan 1
  set new-connection-chance 0
end

to go
  if not any? turtles
    [ stop ]
  if all? turtles [not infected?]
    [ if Stop-with-virus-extinction
        [stop]]
  ask turtles
  [
     set virus-check-timer virus-check-timer + 1
     if virus-check-timer >= virus-check-frequency
       [ set virus-check-timer 0 ]

     set vaccine-check-timer vaccine-check-timer + 1
     if vaccine-check-timer >= vaccination-check-frequency
       [ set vaccine-check-timer 0 ]
  ]
  if dynamic-network?
    [if not any? turtles [ stop ]
    manage-churn ]
  spread-virus
  do-virus-checks
  do-vaccine-checks
  tick
end

to become-infected [ virus-string ] ;; turtle procedure
  set infected? true
  set infected-with virus-string
  set resistant-to (remove virus-string resistant-to)

  ;; NEW CODE
  ; let virus-genes map [read-from-string item ? virus-string] n-values length virus-string [?]
  ; let virus-strength reduce + virus-genes
  ; set decay-rate ifelse-value (member? virus-string vaccines)
  ;   [ 1 ] [ 1 + (virus-strength  * round (virus-damage-rate / 100)) ]
  ;; OLD CODE
  set decay-rate ifelse-value (member? virus-string vaccines)
    [ 1 ] [ 1 + round (virus-damage-rate / 100) ]

  paint-infected virus-string
end

to become-susceptible [ virus-string ] ;; turtle procedure
  set infected? false
  set infected-with ""
  set resistant-to (remove virus-string resistant-to)
  set decay-rate 1

  paint-not-infected virus-string "susceptible"
end

to become-resistant [ virus-string ] ;; turtle procedure
  set infected? false
  set infected-with ""
  set resistant-to (lput virus-string resistant-to)
  set decay-rate 1

  paint-not-infected virus-string "resistant"
  if (length vaccines = 2)
    [ ask my-links [ set color disabled-edge-color ] ]
end

to do-vaccination [ virus-string ] ;; turtle procedure
  set vaccines (lput virus-string vaccines)
  set shape "person vaccinated"
end

to manage-churn
  ; make turtles age
  ask turtles [
    ;; NEW CODE
    ; set time-to-live (time-to-live - decay-rate)
    ;; OLD CODE
    let decay ifelse-value (infected?)
      [ round (time-to-live * virus-damage-rate / 100) ][ 0 ]
    set time-to-live (time-to-live - (1 + decay))
  ]

  ; kill old turtles
  ask turtles with [time-to-live <= 0] [ die ]

  ; breed new turtles
  set hatch-timer (hatch-timer - 1)
  if (hatch-timer = 0) [
    set hatch-timer average-turtle-birthrate
    create-turtles random ((number-of-nodes) / 2) [ spawn-node ]
    let num-links (average-node-degree * count turtles) / 2
    ask turtles with [count my-links < num-links] [spawn-links]
  ]

  if (random 100 < new-connection-chance) [ spawn-links ]
end

to spread-virus
  ask turtles with [infected? and not quarantined?] [
    let virus-string ifelse-value
      ((not mutation?) and (random 100 < virus-mutation-chance))
      [ do-virus-mutation infected-with ]
      [ infected-with ]

    ask link-neighbors with [not member? virus-string resistant-to] [
      let lessener ifelse-value (member? virus-string vaccines)
        [precision (1 - (vaccine-effectiveness / 100)) 2] [1]
      let chance (virus-spread-chance * lessener)

      if random-float 100 < precision chance 0 [
        set mutation? true
        set known-viruses (remove-duplicates (lput virus-string known-viruses))
        become-infected virus-string

        ask turtles with [ not infected? ] [
          ifelse (length resistant-to > 0)
            [ paint-not-infected virus-string "resistant"]
            [ paint-not-infected virus-string "susceptible" ]
        ]
      ]
    ]
  ]

end

to do-virus-checks
  ask turtles with [infected? and virus-check-timer = 0]
  [
    let booster ifelse-value (member? infected-with vaccines)
      [ precision (1 + (vaccine-effectiveness / 100)) 2] [1]

    if random 100 < precision (recovery-chance * booster) 0
    [
      let chance (gain-resistance-chance * booster)
      ifelse random 100 < precision chance 0
        [ become-resistant infected-with ]
        [ become-susceptible infected-with ]
    ]
  ]
  ask turtles with [virus-check-timer = 0]
  [
    ask link-neighbors with [quarantined? and not infected?]
    [
      set quarantined? false
      ask my-links [set color normal-edge-color]
    ]
    ask link-neighbors with [infected?]
    [
      set quarantined? set-in-isolation?
      if(quarantined?) [ ask my-links [set color quarantined-edge-color] ]
    ]
  ]
end

to do-vaccine-checks
  if (vaccination-rate-per-cycle > 0) [
    let n-turtles round (count turtles * vaccination-rate-per-cycle / 100)
    ask n-of n-turtles turtles
    [
      if ((random 100 < vaccination-chance) and
        (vaccines != available-vaccines) and
        (vaccine-check-timer = 0))
      [ do-vaccination item 0 available-vaccines ]
    ]
  ]
end

to-report spawn-new-virus
  let counter 6
  let virus-string []
  while [ counter > 0 ] [
    set virus-string (lput (random 2) virus-string)
    set counter (counter - 1)
  ]
  report reduce word virus-string
end

to-report do-virus-mutation [ virus-string ]
  let indices n-values length virus-string [?]
  let new-virus-string map [read-from-string item ? virus-string] indices
  foreach indices [
    if (random 100 < virus-mutation-chance) [
      set new-virus-string replace-item ? new-virus-string (1 - (item ? new-virus-string));
    ]
  ]
  report reduce word new-virus-string
end

to paint-infected [ virus-string ]
  set shape ifelse-value (length vaccines = 0)
    ["person"]["person vaccinated"]
  let offset 3 * (position virus-string known-viruses)
  set color infected-color + offset
end

to paint-not-infected  [ virus-string turtle-state ]
  let shape-name ifelse-value (length vaccines = 0)
    ["person"]["person vaccinated"]
  set shape-name ifelse-value (mutation?)
    [word shape-name word " half " turtle-state ][shape-name]
  set shape shape-name

  let offset ifelse-value (length known-viruses = 1) [0][3]
  set color ifelse-value (member? virus-string resistant-to)
    [ resistant-color + 1.5 * offset ]
    [ susceptible-color + offset ]
end


; Copyright 2008 Uri Wilensky.
; See Info tab for full copyright and license.
@#$#@#$#@
GRAPHICS-WINDOW
445
29
906
511
20
20
11.0
1
10
1
1
1
0
0
0
1
-20
20
-20
20
1
1
1
ticks
30.0

SLIDER
226
94
431
127
gain-resistance-chance
gain-resistance-chance
0.0
100
0
1
1
%
HORIZONTAL

SLIDER
226
62
431
95
recovery-chance
recovery-chance
0
100
5
1
1
%
HORIZONTAL

SLIDER
9
330
214
363
virus-spread-chance
virus-spread-chance
0.0
10.0
10
0.1
1
%
HORIZONTAL

BUTTON
676
515
791
548
NIL
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
791
515
906
548
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
925
30
1801
511
Network Status
time
# of nodes
0.0
52.0
0.0
100.0
true
true
"" ""
PENS
"susceptible" 1.0 0 -10899396 true "" "plot (count turtles with [not infected? and length resistant-to < length known-viruses])"
"infected" 1.0 0 -2674135 true "" "plot (count turtles with [infected?])"
"vaccinated" 1.0 0 -13791810 true "" "plot (count turtles with [length vaccines > 0])"
"resistant" 1.0 0 -7500403 true "" "plot (count turtles with [length resistant-to > 0])"
"population" 1.0 0 -955883 true "" "plot (count turtles)"

SLIDER
9
30
214
63
number-of-nodes
number-of-nodes
10
300
300
5
1
NIL
HORIZONTAL

SLIDER
9
363
214
396
virus-check-frequency
virus-check-frequency
1
20
1
1
1
ticks
HORIZONTAL

SLIDER
9
297
214
330
initial-outbreak-size
initial-outbreak-size
1
number-of-nodes
30
1
1
NIL
HORIZONTAL

SLIDER
9
62
214
95
average-node-degree
average-node-degree
1
number-of-nodes - 1
6
1
1
NIL
HORIZONTAL

SLIDER
226
161
431
194
initial-vaccination-size
initial-vaccination-size
0
number-of-nodes
270
1
1
NIL
HORIZONTAL

SLIDER
226
259
431
292
vaccine-effectiveness
vaccine-effectiveness
0
100
95
1
1
%
HORIZONTAL

SLIDER
226
30
431
63
initial-resistance-size
initial-resistance-size
0
number-of-nodes
0
1
1
NIL
HORIZONTAL

SLIDER
9
162
214
195
average-turtle-lifespan
average-turtle-lifespan
1
1500
1500
1
1
ticks
HORIZONTAL

SLIDER
9
227
214
260
new-connection-chance
new-connection-chance
0
100
20
1
1
%
HORIZONTAL

SLIDER
9
397
214
430
virus-damage-rate
virus-damage-rate
0
100
25
1
1
%
HORIZONTAL

SLIDER
226
128
431
161
vaccination-check-frequency
vaccination-check-frequency
1
20
1
1
1
ticks
HORIZONTAL

TEXTBOX
15
272
165
291
Virus
15
0.0
1

TEXTBOX
226
10
376
29
Immunity
15
0.0
1

TEXTBOX
9
10
159
29
Network
15
0.0
1

SWITCH
9
130
214
163
dynamic-network?
dynamic-network?
0
1
-1000

BUTTON
445
515
560
548
default settings
reset-default-configs
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
560
515
675
548
reset network
reset
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

SWITCH
9
98
214
131
directed-links?
directed-links?
1
1
-1000

SLIDER
226
226
431
259
vaccination-chance
vaccination-chance
0
100
0
1
1
%
HORIZONTAL

SLIDER
226
194
431
227
vaccination-rate-per-cycle
vaccination-rate-per-cycle
0
100
0
1
1
%
HORIZONTAL

SLIDER
9
194
214
227
average-turtle-birthrate
average-turtle-birthrate
0
1500
1500
1
1
ticks
HORIZONTAL

SLIDER
9
429
214
462
virus-mutation-chance
virus-mutation-chance
0
100
0
1
1
NIL
HORIZONTAL

SWITCH
226
291
431
324
set-in-isolation?
set-in-isolation?
1
1
-1000

SWITCH
923
513
1121
546
Stop-with-virus-extinction
Stop-with-virus-extinction
0
1
-1000

TEXTBOX
196
614
306
658
Presentation support
18
0.0
1

TEXTBOX
315
581
369
608
Static\n
18
0.0
1

TEXTBOX
315
624
397
646
Dynamic
18
0.0
1

BUTTON
444
572
507
605
BASE
Set vaccination-check-frequency 1\nSet virus-check-frequency 1\nSet dynamic-network? false\nSet vaccination-rate-per-cycle 0\nSet initial-vaccination-size 0\nSet new-connection-chance 20\nSet virus-damage-rate 0\nSet recovery-chance 10\nSet vaccination-chance 0\nSet initial-resistance-size 0\nSet average-turtle-birthrate 30\nSet virus-mutation-chance 0\nSet average-turtle-lifespan 50\nSet gain-resistance-chance 10\nSet set-in-isolation? false\nSet average-node-degree 6\nSet number-of-nodes 150\nSet virus-spread-chance 10\nSet vaccine-effectiveness 0\nSet directed-links? false\nSet initial-outbreak-size 15\n\nsetup\n
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
509
572
602
605
Nat. Resist
Set vaccination-check-frequency 1\nSet virus-check-frequency 1\nSet dynamic-network? false\nSet vaccination-rate-per-cycle 0\nSet initial-vaccination-size 0\nSet new-connection-chance 20\nSet virus-damage-rate 0\nSet recovery-chance 10\nSet vaccination-chance 0\nSet initial-resistance-size 50\nSet average-turtle-birthrate 30\nSet virus-mutation-chance 0\nSet average-turtle-lifespan 50\nSet gain-resistance-chance 10\nSet set-in-isolation? false\nSet average-node-degree 6\nSet number-of-nodes 150\nSet virus-spread-chance 10\nSet vaccine-effectiveness 0\nSet directed-links? false\nSet initial-outbreak-size 15\n\nsetup\n
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
604
572
698
605
Vaccination
Set vaccination-check-frequency 1\nSet virus-check-frequency 1\nSet dynamic-network? false\nSet vaccination-rate-per-cycle 10\nSet initial-vaccination-size 0\nSet new-connection-chance 20\nSet virus-damage-rate 0\nSet recovery-chance 10\nSet vaccination-chance 10\nSet initial-resistance-size 0\nSet average-turtle-birthrate 30\nSet virus-mutation-chance 0\nSet average-turtle-lifespan 50\nSet gain-resistance-chance 10\nSet set-in-isolation? false\nSet average-node-degree 6\nSet number-of-nodes 150\nSet virus-spread-chance 10\nSet vaccine-effectiveness 75\nSet directed-links? false\nSet initial-outbreak-size 15\n\nsetup
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
700
572
780
605
Mutation
Set vaccination-check-frequency 1\nSet virus-check-frequency 1\nSet dynamic-network? false\nSet vaccination-rate-per-cycle 0\nSet initial-vaccination-size 0\nSet new-connection-chance 20\nSet virus-damage-rate 0\nSet recovery-chance 10\nSet vaccination-chance 0\nSet initial-resistance-size 0\nSet average-turtle-birthrate 30\nSet virus-mutation-chance 50\nSet average-turtle-lifespan 50\nSet gain-resistance-chance 10\nSet set-in-isolation? false\nSet average-node-degree 6\nSet number-of-nodes 150\nSet virus-spread-chance 10\nSet vaccine-effectiveness 0\nSet directed-links? false\nSet initial-outbreak-size 15\n\nsetup
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
443
617
506
650
BASE
Set vaccination-check-frequency 1\nSet virus-check-frequency 1\nSet dynamic-network? true\nSet vaccination-rate-per-cycle 0\nSet initial-vaccination-size 0\nSet new-connection-chance 20\nSet virus-damage-rate 0\nSet recovery-chance 10\nSet vaccination-chance 0\nSet initial-resistance-size 0\nSet average-turtle-birthrate 20\nSet virus-mutation-chance 0\nSet average-turtle-lifespan 50\nSet gain-resistance-chance 10\nSet set-in-isolation? false\nSet average-node-degree 6\nSet number-of-nodes 150\nSet virus-spread-chance 10\nSet vaccine-effectiveness 0\nSet directed-links? false\nSet initial-outbreak-size 15\n\nsetup
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
508
617
601
650
Nat. Resist
Set vaccination-check-frequency 1\nSet virus-check-frequency 1\nSet dynamic-network? true\nSet vaccination-rate-per-cycle 0\nSet initial-vaccination-size 0\nSet new-connection-chance 20\nSet virus-damage-rate 0\nSet recovery-chance 10\nSet vaccination-chance 0\nSet initial-resistance-size 50\nSet average-turtle-birthrate 20\nSet virus-mutation-chance 0\nSet average-turtle-lifespan 50\nSet gain-resistance-chance 10\nSet set-in-isolation? false\nSet average-node-degree 6\nSet number-of-nodes 150\nSet virus-spread-chance 10\nSet vaccine-effectiveness 0\nSet directed-links? false\nSet initial-outbreak-size 15\n\nsetup
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
603
617
697
650
Vaccination
Set vaccination-check-frequency 1\nSet virus-check-frequency 1\nSet dynamic-network? true\nSet vaccination-rate-per-cycle 10\nSet initial-vaccination-size 0\nSet new-connection-chance 20\nSet virus-damage-rate 0\nSet recovery-chance 10\nSet vaccination-chance 10\nSet initial-resistance-size 0\nSet average-turtle-birthrate 20\nSet virus-mutation-chance 0\nSet average-turtle-lifespan 50\nSet gain-resistance-chance 10\nSet set-in-isolation? false\nSet average-node-degree 6\nSet number-of-nodes 150\nSet virus-spread-chance 10\nSet vaccine-effectiveness 75\nSet directed-links? false\nSet initial-outbreak-size 15\n\nsetup
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
698
617
778
650
Mutation
Set vaccination-check-frequency 1\nSet virus-check-frequency 1\nSet dynamic-network? true\nSet vaccination-rate-per-cycle 0\nSet initial-vaccination-size 0\nSet new-connection-chance 20\nSet virus-damage-rate 0\nSet recovery-chance 10\nSet vaccination-chance 0\nSet initial-resistance-size 0\nSet average-turtle-birthrate 20\nSet virus-mutation-chance 50\nSet average-turtle-lifespan 50\nSet gain-resistance-chance 10\nSet set-in-isolation? false\nSet average-node-degree 6\nSet number-of-nodes 150\nSet virus-spread-chance 10\nSet vaccine-effectiveness 0\nSet directed-links? false\nSet initial-outbreak-size 15\n\nsetup
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
779
617
856
650
Damage
Set vaccination-check-frequency 1\nSet virus-check-frequency 1\nSet dynamic-network? true\nSet vaccination-rate-per-cycle 0\nSet initial-vaccination-size 0\nSet new-connection-chance 20\nSet virus-damage-rate 25\nSet recovery-chance 10\nSet vaccination-chance 0\nSet initial-resistance-size 0\nSet average-turtle-birthrate 20\nSet virus-mutation-chance 0\nSet average-turtle-lifespan 50\nSet gain-resistance-chance 10\nSet set-in-isolation? false\nSet average-node-degree 6\nSet number-of-nodes 150\nSet virus-spread-chance 10\nSet vaccine-effectiveness 0\nSet directed-links? false\nSet initial-outbreak-size 15\n\nsetup
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
858
617
952
650
Quarantine
Set vaccination-check-frequency 1\nSet virus-check-frequency 1\nSet dynamic-network? true\nSet vaccination-rate-per-cycle 0\nSet initial-vaccination-size 0\nSet new-connection-chance 20\nSet virus-damage-rate 0\nSet recovery-chance 10\nSet vaccination-chance 0\nSet initial-resistance-size 0\nSet average-turtle-birthrate 20\nSet virus-mutation-chance 0\nSet average-turtle-lifespan 50\nSet gain-resistance-chance 10\nSet set-in-isolation? true\nSet average-node-degree 6\nSet number-of-nodes 150\nSet virus-spread-chance 10\nSet vaccine-effectiveness 0\nSet directed-links? false\nSet initial-outbreak-size 15\n\nsetup
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

TEXTBOX
312
666
462
688
Herd immunity
18
0.0
1

BUTTON
441
664
504
697
Static
Set vaccination-check-frequency 1\nSet virus-check-frequency 1\nSet dynamic-network? false\nSet vaccination-rate-per-cycle 0\nSet initial-vaccination-size 30\nSet new-connection-chance 20\nSet virus-damage-rate 25\nSet recovery-chance 5\nSet vaccination-chance 0\nSet initial-resistance-size 0\nSet average-turtle-birthrate 1500\nSet virus-mutation-chance 0\nSet average-turtle-lifespan 1500\nSet gain-resistance-chance 0\nSet set-in-isolation? false\nSet average-node-degree 6\nSet number-of-nodes 300\nSet virus-spread-chance 10\nSet vaccine-effectiveness 95\nSet directed-links? false\nSet initial-outbreak-size 30\n\nsetup
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
506
664
584
697
Dynamic
Set vaccination-check-frequency 1\nSet virus-check-frequency 1\nSet dynamic-network? true\nSet vaccination-rate-per-cycle 0\nSet initial-vaccination-size 30\nSet new-connection-chance 20\nSet virus-damage-rate 25\nSet recovery-chance 5\nSet vaccination-chance 0\nSet initial-resistance-size 0\nSet average-turtle-birthrate 1500\nSet virus-mutation-chance 0\nSet average-turtle-lifespan 1500\nSet gain-resistance-chance 0\nSet set-in-isolation? false\nSet average-node-degree 6\nSet number-of-nodes 300\nSet virus-spread-chance 10\nSet vaccine-effectiveness 95\nSet directed-links? false\nSet initial-outbreak-size 30\n\nsetup
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

@#$#@#$#@
## WHAT IS IT?

This model demonstrates the spread of a virus through a network.  Although the model is somewhat abstract, one interpretation is that each node represents a computer, and we are modeling the progress of a computer virus (or worm) through this network.  Each node may be in one of three states:  susceptible, infected, or resistant.  In the academic literature such a model is sometimes referred to as an SIR model for epidemics.

## HOW IT WORKS

Each time step (tick), each infected node (colored red) attempts to infect all of its neighbors.  Susceptible neighbors (colored green) will be infected with a probability given by the VIRUS-SPREAD-CHANCE slider.  This might correspond to the probability that someone on the susceptible system actually executes the infected email attachment.
Resistant nodes (colored gray) cannot be infected.  This might correspond to up-to-date antivirus software and security patches that make a computer immune to this particular virus.

Infected nodes are not immediately aware that they are infected.  Only every so often (determined by the VIRUS-CHECK-FREQUENCY slider) do the nodes check whether they are infected by a virus.  This might correspond to a regularly scheduled virus-scan procedure, or simply a human noticing something fishy about how the computer is behaving.  When the virus has been detected, there is a probability that the virus will be removed (determined by the RECOVERY-CHANCE slider).

If a node does recover, there is some probability that it will become resistant to this virus in the future (given by the GAIN-RESISTANCE-CHANCE slider).

When a node becomes resistant, the links between it and its neighbors are darkened, since they are no longer possible vectors for spreading the virus.

## HOW TO USE IT

Using the sliders, choose the NUMBER-OF-NODES and the AVERAGE-NODE-DEGREE (average number of links coming out of each node).

The network that is created is based on proximity (Euclidean distance) between nodes.  A node is randomly chosen and connected to the nearest node that it is not already connected to.  This process is repeated until the network has the correct number of links to give the specified average node degree.

The INITIAL-OUTBREAK-SIZE slider determines how many of the nodes will start the simulation infected with the virus.

Then press SETUP to create the network.  Press GO to run the model.  The model will stop running once the virus has completely died out.

The VIRUS-SPREAD-CHANCE, VIRUS-CHECK-FREQUENCY, RECOVERY-CHANCE, and GAIN-RESISTANCE-CHANCE sliders (discussed in "How it Works" above) can be adjusted before pressing GO, or while the model is running.

The NETWORK STATUS plot shows the number of nodes in each state (S, I, R) over time.

## THINGS TO NOTICE

At the end of the run, after the virus has died out, some nodes are still susceptible, while others have become immune.  What is the ratio of the number of immune nodes to the number of susceptible nodes?  How is this affected by changing the AVERAGE-NODE-DEGREE of the network?

## THINGS TO TRY

Set GAIN-RESISTANCE-CHANCE to 0%.  Under what conditions will the virus still die out?   How long does it take?  What conditions are required for the virus to live?  If the RECOVERY-CHANCE is bigger than 0, even if the VIRUS-SPREAD-CHANCE is high, do you think that if you could run the model forever, the virus could stay alive?

## EXTENDING THE MODEL

The real computer networks on which viruses spread are generally not based on spatial proximity, like the networks found in this model.  Real computer networks are more often found to exhibit a "scale-free" link-degree distribution, somewhat similar to networks created using the Preferential Attachment model.  Try experimenting with various alternative network structures, and see how the behavior of the virus differs.

Suppose the virus is spreading by emailing itself out to everyone in the computer's address book.  Since being in someone's address book is not a symmetric relationship, change this model to use directed links instead of undirected links.

Can you model multiple viruses at the same time?  How would they interact?  Sometimes if a computer has a piece of malware installed, it is more vulnerable to being infected by more malware.

Try making a model similar to this one, but where the virus has the ability to mutate itself.  Such self-modifying viruses are a considerable threat to computer security, since traditional methods of virus signature identification may not work against them.  In your model, nodes that become immune may be reinfected if the virus has mutated to become significantly different than the variant that originally infected the node.

## RELATED MODELS

Virus, Disease, Preferential Attachment, Diffusion on a Directed Network

## NETLOGO FEATURES

Links are used for modeling the network.  The `layout-spring` primitive is used to position the nodes and links such that the structure of the network is visually clear.

Though it is not used in this model, there exists a network extension for NetLogo that you can download at: https://github.com/NetLogo/NW-Extension.

## HOW TO CITE

If you mention this model or the NetLogo software in a publication, we ask that you include the citations below.

For the model itself:

* Stonedahl, F. and Wilensky, U. (2008).  NetLogo Virus on a Network model.  http://ccl.northwestern.edu/netlogo/models/VirusonaNetwork.  Center for Connected Learning and Computer-Based Modeling, Northwestern University, Evanston, IL.

Please cite the NetLogo software as:

* Wilensky, U. (1999). NetLogo. http://ccl.northwestern.edu/netlogo/. Center for Connected Learning and Computer-Based Modeling, Northwestern University, Evanston, IL.

## COPYRIGHT AND LICENSE

Copyright 2008 Uri Wilensky.

![CC BY-NC-SA 3.0](http://ccl.northwestern.edu/images/creativecommons/byncsa.png)

This work is licensed under the Creative Commons Attribution-NonCommercial-ShareAlike 3.0 License.  To view a copy of this license, visit https://creativecommons.org/licenses/by-nc-sa/3.0/ or send a letter to Creative Commons, 559 Nathan Abbott Way, Stanford, California 94305, USA.

Commercial licenses are also available. To inquire about commercial licenses, please contact Uri Wilensky at uri@northwestern.edu.

<!-- 2008 Cite: Stonedahl, F. -->
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

building institution
false
0
Rectangle -7500403 true true 0 60 300 270
Rectangle -16777216 true false 130 196 168 256
Rectangle -16777216 false false 0 255 300 270
Polygon -7500403 true true 0 60 150 15 300 60
Polygon -16777216 false false 0 60 150 15 300 60
Circle -1 true false 135 26 30
Circle -16777216 false false 135 25 30
Rectangle -16777216 false false 0 60 300 75
Rectangle -16777216 false false 218 75 255 90
Rectangle -16777216 false false 218 240 255 255
Rectangle -16777216 false false 224 90 249 240
Rectangle -16777216 false false 45 75 82 90
Rectangle -16777216 false false 45 240 82 255
Rectangle -16777216 false false 51 90 76 240
Rectangle -16777216 false false 90 240 127 255
Rectangle -16777216 false false 90 75 127 90
Rectangle -16777216 false false 96 90 121 240
Rectangle -16777216 false false 179 90 204 240
Rectangle -16777216 false false 173 75 210 90
Rectangle -16777216 false false 173 240 210 255
Rectangle -16777216 false false 269 90 294 240
Rectangle -16777216 false false 263 75 300 90
Rectangle -16777216 false false 263 240 300 255
Rectangle -16777216 false false 0 240 37 255
Rectangle -16777216 false false 6 90 31 240
Rectangle -16777216 false false 0 75 37 90
Line -16777216 false 112 260 184 260
Line -16777216 false 105 265 196 265

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

computer workstation
false
0
Rectangle -7500403 true true 60 45 240 180
Polygon -7500403 true true 90 180 105 195 135 195 135 210 165 210 165 195 195 195 210 180
Rectangle -16777216 true false 75 60 225 165
Rectangle -7500403 true true 45 210 255 255
Rectangle -10899396 true false 249 223 237 217
Line -16777216 false 60 225 120 225

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

person half resistant
false
15
Circle -7500403 true false 110 5 80
Polygon -7500403 true false 105 90 120 195 90 285 105 300 135 300 150 225 165 300 195 300 210 285 180 195 195 90
Rectangle -7500403 true false 127 79 172 94
Polygon -7500403 true false 195 90 240 150 225 180 165 105
Polygon -1 true true 105 90 60 150 75 180 120 120
Polygon -1 true true 105 90 180 195 210 285 195 300 165 300 150 225 135 300 105 300 90 285 120 195 105 90

person half susceptible
false
15
Circle -10899396 true false 110 5 80
Polygon -10899396 true false 105 90 120 195 90 285 105 300 135 300 150 225 165 300 195 300 210 285 180 195 195 90
Rectangle -10899396 true false 127 79 172 94
Polygon -10899396 true false 195 90 240 150 225 180 165 105
Polygon -1 true true 105 90 60 150 75 180 120 120
Polygon -1 true true 105 90 180 195 210 285 195 300 165 300 150 225 135 300 105 300 90 285 120 195 105 90

person vaccinated
false
0
Circle -7500403 true true 110 5 80
Polygon -7500403 true true 105 90 120 195 90 285 105 300 135 300 150 225 165 300 195 300 210 285 180 195 195 90
Rectangle -7500403 true true 127 79 172 94
Polygon -7500403 true true 195 90 240 150 225 180 165 105
Polygon -7500403 true true 105 90 60 150 75 180 135 105
Line -7500403 true 135 90 165 90
Rectangle -1184463 true false 75 105 225 165
Rectangle -1184463 true false 120 60 180 210
Polygon -16777216 false false 120 60 180 60 180 105 225 105 225 165 180 165 180 210 120 210 120 165 75 165 75 105 120 105 120 60

person vaccinated half resistant
false
15
Circle -7500403 true false 110 5 80
Polygon -7500403 true false 105 90 120 195 90 285 105 300 135 300 150 225 165 300 195 300 210 285 180 195 195 90
Rectangle -7500403 true false 127 79 172 94
Polygon -7500403 true false 195 90 240 150 225 180 165 105
Polygon -1 true true 105 90 60 150 75 180 120 120
Polygon -1 true true 105 90 180 195 210 285 195 300 165 300 150 225 135 300 105 300 90 285 120 195 105 90
Rectangle -1184463 true false 120 60 180 210
Rectangle -1184463 true false 75 105 225 165
Polygon -16777216 false false 180 60 180 105 225 105 225 165 180 165 180 210 120 210 120 165 120 165 75 165 75 105 120 105 120 60

person vaccinated half susceptible
false
15
Circle -10899396 true false 110 5 80
Polygon -10899396 true false 105 90 120 195 90 285 105 300 135 300 150 225 165 300 195 300 210 285 180 195 195 90
Rectangle -10899396 true false 127 79 172 94
Polygon -10899396 true false 195 90 240 150 225 180 165 105
Polygon -1 true true 105 90 60 150 75 180 120 120
Polygon -1 true true 105 90 180 195 210 285 195 300 165 300 150 225 135 300 105 300 90 285 120 195 105 90
Rectangle -1184463 true false 120 60 180 210
Rectangle -1184463 true false 75 105 225 165
Polygon -16777216 false false 180 60 180 105 225 105 225 165 180 165 180 165 180 210 120 210 120 165 75 165 75 105 120 105 120 60

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

x
false
0
Polygon -7500403 true true 270 75 225 30 30 225 75 270
Polygon -7500403 true true 30 75 75 30 270 225 225 270

@#$#@#$#@
NetLogo 5.3.1
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
<experiments>
  <experiment name="Static - Base scenario" repetitions="1000" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <exitCondition>all? turtles [not infected?]</exitCondition>
    <metric>count turtles</metric>
    <enumeratedValueSet variable="vaccination-check-frequency">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="virus-check-frequency">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="dynamic-network?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="vaccination-rate-per-cycle">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-vaccination-size">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="new-connection-chance">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="virus-damage-rate">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="recovery-chance">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="vaccination-chance">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-resistance-size">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="average-turtle-birthrate">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="virus-mutation-chance">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="average-turtle-lifespan">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="gain-resistance-chance">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="set-in-isolation?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="average-node-degree">
      <value value="6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-of-nodes">
      <value value="150"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="virus-spread-chance">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="vaccine-efficiency">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="directed-links?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-outbreak-size">
      <value value="15"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="Static - Natural Resistance" repetitions="1000" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <exitCondition>all? turtles [not infected?]</exitCondition>
    <metric>count turtles</metric>
    <enumeratedValueSet variable="vaccination-check-frequency">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="virus-check-frequency">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="dynamic-network?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="vaccination-rate-per-cycle">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-vaccination-size">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="new-connection-chance">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="virus-damage-rate">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="recovery-chance">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="vaccination-chance">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-resistance-size">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="average-turtle-birthrate">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="virus-mutation-chance">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="average-turtle-lifespan">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="gain-resistance-chance">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="set-in-isolation?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="average-node-degree">
      <value value="6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-of-nodes">
      <value value="150"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="virus-spread-chance">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="vaccine-efficiency">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="directed-links?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-outbreak-size">
      <value value="15"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="Static - Immunization" repetitions="1000" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <exitCondition>all? turtles [not infected?]</exitCondition>
    <metric>count turtles</metric>
    <enumeratedValueSet variable="vaccination-check-frequency">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="virus-check-frequency">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="dynamic-network?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="vaccination-rate-per-cycle">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-vaccination-size">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="new-connection-chance">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="virus-damage-rate">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="recovery-chance">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="vaccination-chance">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-resistance-size">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="average-turtle-birthrate">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="virus-mutation-chance">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="average-turtle-lifespan">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="gain-resistance-chance">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="set-in-isolation?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="average-node-degree">
      <value value="6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-of-nodes">
      <value value="150"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="virus-spread-chance">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="vaccine-efficiency">
      <value value="75"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="directed-links?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-outbreak-size">
      <value value="15"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="Static - Virus Mutation" repetitions="1000" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <exitCondition>all? turtles [not infected?]</exitCondition>
    <metric>count turtles</metric>
    <enumeratedValueSet variable="vaccination-check-frequency">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="virus-check-frequency">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="dynamic-network?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="vaccination-rate-per-cycle">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-vaccination-size">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="new-connection-chance">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="virus-damage-rate">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="recovery-chance">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="vaccination-chance">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-resistance-size">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="average-turtle-birthrate">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="virus-mutation-chance">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="average-turtle-lifespan">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="gain-resistance-chance">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="set-in-isolation?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="average-node-degree">
      <value value="6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-of-nodes">
      <value value="150"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="virus-spread-chance">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="vaccine-efficiency">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="directed-links?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-outbreak-size">
      <value value="15"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="Dynamic - Base scenario" repetitions="1000" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <exitCondition>all? turtles [not infected?]</exitCondition>
    <metric>count turtles</metric>
    <enumeratedValueSet variable="vaccination-check-frequency">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="virus-check-frequency">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="dynamic-network?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="vaccination-rate-per-cycle">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-vaccination-size">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="new-connection-chance">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="virus-damage-rate">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="recovery-chance">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="vaccination-chance">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-resistance-size">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="average-turtle-birthrate">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="virus-mutation-chance">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="average-turtle-lifespan">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="gain-resistance-chance">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="set-in-isolation?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="average-node-degree">
      <value value="6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-of-nodes">
      <value value="150"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="virus-spread-chance">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="vaccine-efficiency">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="directed-links?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-outbreak-size">
      <value value="15"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="Dynamic - Natural Resistance" repetitions="1000" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <exitCondition>all? turtles [not infected?]</exitCondition>
    <metric>count turtles</metric>
    <enumeratedValueSet variable="vaccination-check-frequency">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="virus-check-frequency">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="dynamic-network?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="vaccination-rate-per-cycle">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-vaccination-size">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="new-connection-chance">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="virus-damage-rate">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="recovery-chance">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="vaccination-chance">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-resistance-size">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="average-turtle-birthrate">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="virus-mutation-chance">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="average-turtle-lifespan">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="gain-resistance-chance">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="set-in-isolation?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="average-node-degree">
      <value value="6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-of-nodes">
      <value value="150"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="virus-spread-chance">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="vaccine-efficiency">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="directed-links?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-outbreak-size">
      <value value="15"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="Dynamic - Immunization" repetitions="1000" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <exitCondition>all? turtles [not infected?]</exitCondition>
    <metric>count turtles</metric>
    <enumeratedValueSet variable="vaccination-check-frequency">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="virus-check-frequency">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="dynamic-network?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="vaccination-rate-per-cycle">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-vaccination-size">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="new-connection-chance">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="virus-damage-rate">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="recovery-chance">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="vaccination-chance">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-resistance-size">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="average-turtle-birthrate">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="virus-mutation-chance">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="average-turtle-lifespan">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="gain-resistance-chance">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="set-in-isolation?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="average-node-degree">
      <value value="6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-of-nodes">
      <value value="150"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="virus-spread-chance">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="vaccine-efficiency">
      <value value="75"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="directed-links?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-outbreak-size">
      <value value="15"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="Dynamic - Virus mutation" repetitions="1000" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <exitCondition>all? turtles [not infected?]</exitCondition>
    <metric>count turtles</metric>
    <enumeratedValueSet variable="vaccination-check-frequency">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="virus-check-frequency">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="dynamic-network?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="vaccination-rate-per-cycle">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-vaccination-size">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="new-connection-chance">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="virus-damage-rate">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="recovery-chance">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="vaccination-chance">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-resistance-size">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="average-turtle-birthrate">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="virus-mutation-chance">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="average-turtle-lifespan">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="gain-resistance-chance">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="set-in-isolation?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="average-node-degree">
      <value value="6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-of-nodes">
      <value value="150"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="virus-spread-chance">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="vaccine-efficiency">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="directed-links?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-outbreak-size">
      <value value="15"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="Dynamic - Damage Rate" repetitions="1000" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <exitCondition>all? turtles [not infected?]</exitCondition>
    <metric>count turtles</metric>
    <enumeratedValueSet variable="vaccination-check-frequency">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="virus-check-frequency">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="dynamic-network?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="vaccination-rate-per-cycle">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-vaccination-size">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="new-connection-chance">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="virus-damage-rate">
      <value value="25"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="recovery-chance">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="vaccination-chance">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-resistance-size">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="average-turtle-birthrate">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="virus-mutation-chance">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="average-turtle-lifespan">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="gain-resistance-chance">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="set-in-isolation?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="average-node-degree">
      <value value="6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-of-nodes">
      <value value="150"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="virus-spread-chance">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="vaccine-efficiency">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="directed-links?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-outbreak-size">
      <value value="15"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="Dynamic - Quarantine" repetitions="1000" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <exitCondition>all? turtles [not infected?]</exitCondition>
    <metric>count turtles</metric>
    <enumeratedValueSet variable="vaccination-check-frequency">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="virus-check-frequency">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="dynamic-network?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="vaccination-rate-per-cycle">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-vaccination-size">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="new-connection-chance">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="virus-damage-rate">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="recovery-chance">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="vaccination-chance">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-resistance-size">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="average-turtle-birthrate">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="virus-mutation-chance">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="average-turtle-lifespan">
      <value value="50"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="gain-resistance-chance">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="set-in-isolation?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="average-node-degree">
      <value value="6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-of-nodes">
      <value value="150"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="virus-spread-chance">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="vaccine-efficiency">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="directed-links?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-outbreak-size">
      <value value="15"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="Herd_Static_10%" repetitions="1000" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="200"/>
    <metric>(count turtles with [infected?])</metric>
    <enumeratedValueSet variable="average-node-degree">
      <value value="6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="average-turtle-birthrate">
      <value value="1500"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="vaccination-check-frequency">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="dynamic-network?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Stop-with-virus-extinction">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="recovery-chance">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="virus-check-frequency">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-of-nodes">
      <value value="300"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="virus-spread-chance">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="gain-resistance-chance">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-outbreak-size">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-resistance-size">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-vaccination-size">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="new-connection-chance">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="average-turtle-lifespan">
      <value value="1500"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="vaccination-chance">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="vaccination-rate-per-cycle">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="virus-damage-rate">
      <value value="25"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="directed-links?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="virus-mutation-chance">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="set-in-isolation?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="vaccine-efficiency">
      <value value="95"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="Herd_Static_50%" repetitions="1000" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="200"/>
    <metric>(count turtles with [infected?])</metric>
    <enumeratedValueSet variable="average-node-degree">
      <value value="6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="average-turtle-birthrate">
      <value value="1500"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="vaccination-check-frequency">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="dynamic-network?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Stop-with-virus-extinction">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="recovery-chance">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="virus-check-frequency">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-of-nodes">
      <value value="300"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="virus-spread-chance">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="gain-resistance-chance">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-outbreak-size">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-resistance-size">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-vaccination-size">
      <value value="150"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="new-connection-chance">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="average-turtle-lifespan">
      <value value="1500"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="vaccination-chance">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="vaccination-rate-per-cycle">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="virus-damage-rate">
      <value value="25"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="directed-links?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="virus-mutation-chance">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="set-in-isolation?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="vaccine-efficiency">
      <value value="95"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="Herd_Static_90%" repetitions="1000" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <timeLimit steps="200"/>
    <metric>(count turtles with [infected?])</metric>
    <enumeratedValueSet variable="average-node-degree">
      <value value="6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="average-turtle-birthrate">
      <value value="1500"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="vaccination-check-frequency">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="dynamic-network?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Stop-with-virus-extinction">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="recovery-chance">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="virus-check-frequency">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-of-nodes">
      <value value="300"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="virus-spread-chance">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="gain-resistance-chance">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-outbreak-size">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-resistance-size">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-vaccination-size">
      <value value="270"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="new-connection-chance">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="average-turtle-lifespan">
      <value value="1500"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="vaccination-chance">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="vaccination-rate-per-cycle">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="virus-damage-rate">
      <value value="25"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="directed-links?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="virus-mutation-chance">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="set-in-isolation?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="vaccine-efficiency">
      <value value="95"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="Herd_Dynamic_10%" repetitions="1000" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <exitCondition>all? turtles [not infected?]</exitCondition>
    <metric>count turtles</metric>
    <enumeratedValueSet variable="average-node-degree">
      <value value="6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="average-turtle-birthrate">
      <value value="1500"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="vaccination-check-frequency">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="dynamic-network?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Stop-with-virus-extinction">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="recovery-chance">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="virus-check-frequency">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-of-nodes">
      <value value="300"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="virus-spread-chance">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="gain-resistance-chance">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-outbreak-size">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-resistance-size">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-vaccination-size">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="new-connection-chance">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="average-turtle-lifespan">
      <value value="1500"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="vaccination-chance">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="vaccination-rate-per-cycle">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="virus-damage-rate">
      <value value="25"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="directed-links?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="virus-mutation-chance">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="set-in-isolation?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="vaccine-efficiency">
      <value value="95"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="Herd_Dynamic_50%" repetitions="1000" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <exitCondition>all? turtles [not infected?]</exitCondition>
    <metric>count turtles</metric>
    <enumeratedValueSet variable="average-node-degree">
      <value value="6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="average-turtle-birthrate">
      <value value="1500"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="vaccination-check-frequency">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="dynamic-network?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Stop-with-virus-extinction">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="recovery-chance">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="virus-check-frequency">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-of-nodes">
      <value value="300"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="virus-spread-chance">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="gain-resistance-chance">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-outbreak-size">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-resistance-size">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-vaccination-size">
      <value value="150"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="new-connection-chance">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="average-turtle-lifespan">
      <value value="1500"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="vaccination-chance">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="vaccination-rate-per-cycle">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="virus-damage-rate">
      <value value="25"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="directed-links?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="virus-mutation-chance">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="set-in-isolation?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="vaccine-efficiency">
      <value value="95"/>
    </enumeratedValueSet>
  </experiment>
  <experiment name="Herd_Dynamic_90%" repetitions="1000" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <exitCondition>all? turtles [not infected?]</exitCondition>
    <metric>count turtles</metric>
    <enumeratedValueSet variable="average-node-degree">
      <value value="6"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="average-turtle-birthrate">
      <value value="1500"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="vaccination-check-frequency">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="dynamic-network?">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="Stop-with-virus-extinction">
      <value value="true"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="recovery-chance">
      <value value="5"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="virus-check-frequency">
      <value value="1"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="number-of-nodes">
      <value value="300"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="virus-spread-chance">
      <value value="10"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="gain-resistance-chance">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-outbreak-size">
      <value value="30"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-resistance-size">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="initial-vaccination-size">
      <value value="270"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="new-connection-chance">
      <value value="20"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="average-turtle-lifespan">
      <value value="1500"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="vaccination-chance">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="vaccination-rate-per-cycle">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="virus-damage-rate">
      <value value="25"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="directed-links?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="virus-mutation-chance">
      <value value="0"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="set-in-isolation?">
      <value value="false"/>
    </enumeratedValueSet>
    <enumeratedValueSet variable="vaccine-efficiency">
      <value value="95"/>
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
