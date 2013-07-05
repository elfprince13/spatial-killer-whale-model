; Matthew Dickerson
; v 1.0
; v 2.0  -- Added a graph-based algorithm for a whale to find a path to a given location.


; TO ADD
; -- whale memory of good feeding places (by season? by quality? expiring? intialized?)
; -- socialization (especially fision and fusion)



; This file (whale.nlogo) includes model controls plus whale behavior.

__includes ["map.nls" "prey.nls" "constants.nls" "distribution.nls" "geometry.nls" "memory.nls"]
; ------- ADDITIONAL FILES ----------
;     map.nls includes procedures and declarations for spatial information.
;     constants.nls includes declaration and initialization of constants, primarily related to whale physiology and movement
;     prey.nls includes procedures and declarations for prey data, including loading and updating prey.

; ---------------------BREEDS, GLOBALS, STATE VARIABLES --------------
breed [whales whale]

whales-own [
  female? pregnant? lactating?     ; boolean variables for female whales
  is-nursing?
  mother-id                        ; unique ID of this whale's mother, used for matrilineal lines
  whales-met                       ; a list of whales with which this whale has associated
  energy                           ; keeps track of food consumption vs. metabolism. Food intake increases energy. Every turn causes a decrease.
  is-leader?                       ; set to true if this whale is a group leader
  group                            ; a list of whales in this whale's current group -- only used if is-leader is true.
  leader                           ; the whale who leads this group -- only used if is-leader? is false.
  body-mass
  kcals-consumed
  milk-kgs-produced
  age-years
  age-days
  alive?                            ; boolean
  days-to-birth
  pregnancy-cost
  conception-day
  had-male-child?
  gut-fill                        ; stores kgs consumed and undigested
  ; body-condition
  last-rest                       ; time steps since last rest
  
  destination                     ; a whale's current destination, given as a patch.   
  current-path                    ; If the whale is in travel mode (long distance) then this contains a list of patches giving a path to the whale's destination.
  
  memory
]

globals [
  ; General global variables used for the simulation
  hours days years   ; keeps track of the time
  HpT                ; hours per tick -- determines the time scale
  
  leaders       ; a list of whales that are current group leaders (and decision makers)
    
  INITIAL-NUMBER-WHALES
  USE-DENSITY-DEPENDENCE?      ; if true, over-crowding effects are considered
  SCALE-REPRO-K-AGES-10-12?    ; if true, whale reproductivity is impacted for 10, 11, and 12-year old females
  MIN-HUNTING-K                ; minimum food-rating of a patch (expect number of catchable prey) before whales will consider hunting there.
  
  ; level of text output to the user while the simulation is initialized and run. Each level is inclusive of the previous  
  SETUP-MONITOR-LEVEL          ; 0 = no output.   1 = stages   ; 2 = mid-level      ; 3 = details 
  RUN-MONITOR-LEVEL            ; All output is to a file.  0 = no output   1 = basic decisions   ; 2 = mid-level      ; 3 = details such as prey counts, paths found
  DEBUG-MONITOR-LEVEL
  
  ; stats
  successful-mating
  whales-born
  whales-died-at-birth
  whales-starved
  other-deaths
  
]

; ---------------------INITIALIZATION PROCEDURES--------------
to setup
  ca
  
  set SETUP-MONITOR-LEVEL 2
  set DEBUG-MONITOR-LEVEL 2
  
  ; Initialize the physical world
  load-map                  ; Loads map data from an image, determine which patches are are land, coast, and water.
  
  ; set the distance from each water patch to the nearest coastal patch, and color water accordingly -- darker is farther from shore.
  set-distance-in
  
  
  compute-vd                ; Identify islands. For each water patch, compute the nearest land patch -- this implicitly gives the Voronoi diagram.
  make-graph                ; Create a graph of nodes (including special anchor nodes) to be used for long distance whale movement
  all-anchors-SSSP          ; Compute the shortest path from all graph nodes to all anchor nodes, and store the shortest-path info in the nodes. 
  
  ; Some globals that determine how the simulation is run
  set INITIAL-NUMBER-WHALES 180
  set USE-DENSITY-DEPENDENCE? true
  set SCALE-REPRO-K-AGES-10-12? true
  set MIN-HUNTING-K  1.5

  ; Load data from data files for the prey and the whales
  init-prey  
  init-whales
    
  ; Initialize statistics variables
  set hours 0
  set days 0
  set years 0
  set HpT 1
  set successful-mating 0
  set whales-born 0
  set whales-died-at-birth 0
  set whales-starved 0
  set other-deaths 0
  
  set RUN-MONITOR-LEVEL 2     ; 1 = minimal screen output   2 = file output of hunting events and daily decisions    3 = file output of detailed decisions
    
  let file-name (word "Testrun-" (substring date-and-time 16 27) "-" (substring date-and-time 0 8))
  file-open file-name
  file-print (word "Whale test run on " date-and-time " with " INITIAL-NUMBER-WHALES " whales.") 

  reset-ticks
end

to save-current-world
  export-world "se-alaska-data.csv"
end

;; =========================================================================================================
; Initialization of the population of whales
; In v 1.0, whales ages, genders, etc. are random, and there is one matrilineal line
to init-whales
  SETUP-MONITOR 0 "Initializing whales..."
  init-whale-globals

  ; initialize a random population of whales, 50% female, aged 2 to 24, with no pregnant or lactating females
  create-whales INITIAL-NUMBER-WHALES [
    set whales-met (list )
    set group nobody
    set is-leader? false
    set size 8.0                ; the size is for visibility
    
    ifelse random 2 = 0 [       ; Currently a random population of females and males
      set female? true
      set color magenta + 2
    ][
      set female? false
      set color red
    ]
    set pregnant? false         ; initially assume no pregnant or lactating whales, and none under 2 year old
    set lactating? false
    set days-to-birth -1        ; means the whale is not pregnant
    set age-years random 25
    ifelse age-years < 2 [set is-nursing? true][set is-nursing? false]
    set age-days random 365
    set body-mass (.7 + random-float 0.3) * expected-mass   ; actually body-mass initially between 70% and 100% of the expected mass
    set gut-fill 0
    
    set destination nobody
    set current-path nobody
  ]
  
  ; Assign to whales a random older female as its mother. Every whale should have a mother-id except the oldest females, which are the matriarchs.
  let matriarch-age [age-years] of max-one-of whales with [female?] [age-years]  ; find age of the oldest female
  ask whales [                                                                   ; ask other whales to set their mother-id (unless too old to be offspring of matriach)
    ifelse (age-years + 5) < matriarch-age [                     ; if more than 5 years younger than the matriarch, assign a random older female whale as mother
      set mother-id [who] of one-of other whales with [female? and age-years > 5 + [age-years] of myself]
    ][
      set mother-id -1                                           ; Whales to old to be the offspring of the matriarch no longer have a living mother.
    ]
  ]
  
  set leaders (turtle-set) 
  ; Pick group leaders. To create average group sizes of 3.5, we let 1/3.5 of the whales be leaders.  Leaders should be at least 10 years old.
  ; Leaders will be initialized at some random inland patch.  Other whales assigned to their group will join them.
  ask n-of (INITIAL-NUMBER-WHALES / 3.5) whales with [age-years >= 10] [
    set is-leader? true                              ; identify this whale as a leader who will be making group decisions
    set group (turtle-set self)                      ; put each leader in his own group
    move-to one-of water with [shore-dist < 30 and shore-dist > -1]      ; move to a random patch which isn't "too far out"
  ]
  ask whales with [not is-leader?] [join-random-group]      ; put all whales in some group
  balance-groups                                            ; there should be variance among group sizes, but don't allow groups of just 1 adult
  set leaders whales with [is-leader?]              ; Create a list of group leaders for later efficiency
  ; plot-masses
end

; ----------------------------------------

; CONTEXT: One whale
; A whale joins a random group. *** FOR NOW, DON'T LET GROUP SIZES GET LARGER THAN 6 ***
to join-random-group
   set leader one-of whales with [is-leader?]         ; pick a random leader whale in order to join that whale's group
   move-to [patch-here] of leader                     ; move to the leader's patch
   ask leader [set group (turtle-set group myself)]   ; ask the leader to put you in her group
end

; CONTEXT:  observer
; Make sure that all groups have at least 2 adults
to balance-groups
  ; find leaderes if groups with only 1 adult whale
  ask whales with [is-leader? and count group with [age-years >= 10] < 2][
     disband-group
  ]
end

; CONTEXT:  Whale group leader
; This whale's group initially had only 1 adult male.  It should be disbanded, and the whales moved to other groups.
to disband-group
  set is-leader? false
  ask group [join-random-group]
  set group nobody
end
;==================================================
; ---------------------ONE TICK -------------------

; CONTEXT: Observer
; This is the basic logic procedure for a single time unit.
to move
   tick
   
   ; Track whales only if track-whales switch is set to true 
   ask whales [pen-up]              
   if track-whales [ask leaders [pen-down]]
   
   move-whales                      ; Handles all whale behavior
   update-prey                      ; Updates all prey, including responses to nearby whales, seasonal migration, and population dynamics
   increment-clock
   if (hours = 0) [ask whales [daily-step]]        ; Daily metabolism
end

; CONTEXT: Observer
; Update the global clocks. On tick = HpT hours, where HpT is a global representing hours-per-tick.  This assumes that HpT evenly divides 24.
to increment-clock
   set hours hours + HpT            ; Add a tick's worth of hours
   if hours = 24 [                 ; If past 23 hours, start a new day.  (Hours are numbered 0..23)
     set hours 0
     set days days + 1
     ; plot-masses
   ]
   if days >= 365 [                 ; If past 364 days, start a new year.  (Days are numbered 0..364. There are no leap years modeled.)              
     set days days - 365
     set years years + 1
   ]
end

; ---------------------WHALE MOVEMENT AND DYNAMICS: HIGH LEVEL --------------


; CONTEXT: Observer
; At the start of each turn, we take care of group dynamics (fusion and fision)
; Movement is decided by the leader of each currently existing group 
to move-whales
   RUN-MONITOR 0 "Moving all whales (by calls to group leaders)."
   ask leaders [movement-decisions]
end

; CONTEXT: One whale (should be a group leader)
; This contains the first layer of the basic logic for whale movement -- the agent-based behavior of whales
; This is called only by leaders of the groups, who then direct all whales in their groups to follow.
to movement-decisions
  ; show "making movement decisions"
   ifelse group-tired? [        ; Fatigue takes first precedent in decision making
     rest
   ][                           ; ----- Whales are not tired...
     ifelse group-hungry? [     ; If any whales (in the group) need energy, they will try to get food
       ifelse food-present?     ; If the group is hungry, check whether there is food present on this patch
          [hunt]                ; If there is enough food present for hunting, use statistical models for hunting success
          [seek-food]           ; If not enough food present for hunting, move to a different patch
     ][                         ; ----- Whales are not tired or hungry...
       ifelse meeting?          ; If meeting another group, then socialize, otherwise travel
          [socialize]           
          [travel]             
     ]
  ]
  ask group [set last-rest last-rest + 1]     ; increment counters indicating length of time since last rest
end

; CONTEXT: One whale
; This should be called once per day
; Convert food consumed (kcals-consumed) into body-mass (kgs)
; ... But also decrease body-mass from cals consumed by metabolism
; Age each whale one day
to daily-step
   RUN-MONITOR 1 (word " going through dail step. kcals-consumed = " kcals-consumed ". gut-fill = " gut-fill ". Age = " age-years ":" age-days)
   ; ** MODIFY FOR FEMALES.  
    
   ; Check for death
;   if died-probabilistically? [
;     show " died a random death."
;     die
;   ]
   
   ; FINISH OFF THE CURRENT DAY
   ; Take care of digestion -- converting food consumed into body mass -- and adjust mass based on metabolic needs
   update-mass
   set kcals-consumed 0
   set gut-fill 0
   
   if starved-to-death? [
     RUN-MONITOR 0 " died of starvation."
     RUN-MONITOR 1 (word " died of starvation at age = " age-years ":" age-days ". Current destination: " destination)
     set whales-starved (whales-starved + 1)
     die
   ]
   
   ; Age one day
   set age-days (age-days + 1) 
   if age-days = 365 [
     set age-days 0
     set age-years (age-years + 1)
   ]
   
   ; START A NEW DAY
   update-pregnancy
   if is-nursing? [nurse]         ; All whales that are nursing are asked to nurse.  This starts the new day, and should be done before eating.
end


; ---------------------WHALE MOVEMENT: LOWER LEVEL --------------

; CONTEXT:  One whale (female)
; comment this ****
to update-pregnancy
  RUN-MONITOR 1 " pregnancy update."
  ifelse (days-to-birth > 0) [
    set days-to-birth (days-to-birth - 1)
    set pregnancy-cost pregnancy-cost + fetal-growth ( DAYSPREGNANCY - days-to-birth) * (1 + PREGNANCY-TISSUE-MASS)
    * WHALE-KCAL-PER-KG / ENERGY-TO-FETUS-EFFICIENCY;
  ][  
    ifelse (days-to-birth = 0) [
      give-birth
    ][ ifelse (days = BREEDING-SEASON-START-DAY) [
        ; Beginning of breeding season, assign a pregnancy date using a normal distribution
        ; if we are to get pregnant
        check-begin-conception;
    ][
        if ((days = conception-day)) [
          set conception-day 1000;
          set pregnancy-cost 0;
          ifelse (body-mass >= PREGNANCY-THRESHOLD * expected-mass) [
            ; We just got pregnant
            set days-to-birth DAYSPREGNANCY;
            set successful-mating (successful-mating + 1)
            RUN-MONITOR 1 " is now pregnant."
          ][
            ; We weren't healthy enough...
            RUN-MONITOR 1 " failed to conceive."
          ]
        ]
      ]
    ]
  ]
end

to check-begin-conception
    ; Get probability of reproduction, from tables
    let pRepro (item age-years MX)
        let d random-float 1.0

    if any? whales with [is-nursing? and mother-id = myself][set pRepro 0]

        if (SCALE-REPRO-K-AGES-10-12?) [
            if age-years = 10 [set pRepro (pRepro * .25) ]
            if age-years = 11 [set pRepro (pRepro * .5) ]
            if age-years = 10 [set pRepro (pRepro * .75) ]
        ]


        if USE-DENSITY-DEPENDENCE? [set pRepro pRepro * (1 - count whales / REPRO-K) ]
        if (pRepro < 0) [set pRepro 0]
        

        let conception 0
        if (d < pRepro) [
            set conception random-normal MEAN-DAY-PREGNANT ST-DEV-DAY-PREGNANT
      ]
      
      if (conception <= BREEDING-SEASON-START-DAY) [
                set conception BREEDING-SEASON-START-DAY + 1;
        ]
      
      set conception-day conception;
        RUN-MONITOR 1 (word " might get pregnant on day " conception-day)
end

; Context: One whale, a pregnant female, calls this procedure. 
; A new whale is created, with the given mother and in the mother's group, but with age 0 and a new-born's body mass.
to give-birth
  
  ; update the mother physiology
  set days-to-birth -1                                              ; mother is no longer pregnant
  set body-mass (body-mass - BIRTHMASS * (1 + BIRTH-MASS-LOSS))     ; mass lost by mother in birth

  ; Determine if this whale survives based on survival probabilities
  ; initialize local probability variables
  let pSurv P-NEONATE-SURV        ; probability that the newborn whale survives 1 day
  if USE-DENSITY-DEPENDENCE? [ 
    set pSurv (pSurv * (1 - (count whales) / NEONATE-K)) 
    if pSurv < 0 [set pSurv 0]
  ]   
  let d random-float 1.0                                            ; random number to compare to a probability in order to determine death
  let pDie (1 - pSurv ^ (1.0 / (365.0 / 2.0)))                   ; probability that the newborn whale dies within 1/2 year
         
  
  ifelse d < pDie [
    RUN-MONITOR 1 "Gave birth to a child that does not survive"
    set whales-died-at-birth (whales-died-at-birth + 1)
  ][  
    set whales-born (whales-born + 1)
    ; create the new whale
    hatch 1 [
      set mother-id myself
      set female? (ifelse-value (random 2 = 0) [true][false])         ; 50% chance of being female
      ifelse female?  [
        set color magenta + 2
      ][
        set color red
        ask whale mother-id [set had-male-child? true]
      ]          ; color based on sex
      set body-mass BIRTHMASS     ; standard mass of a newborn
      set gut-fill 0              ; hasn't eaten yet 
      set lactating? false
      set pregnant? false
      set age-years 0
      set age-days 0
      set is-leader? false
      set group nobody
      
      ; The leader of this group is inherited from the mother. However this new whale must be added to that leader's group.
      ask leader [set group (turtle-set group myself)]
      
      ; size, whales-met, heading-out? bearing, and destination are all inherited from mother. So is days-to-birth (which should be -1). 
    ]
  ]

end


; CONTEXT:  One whale
; Nursing whales get milk from their mother. Both the nursing whale and mother are updated.
to nurse
 ; Don't try to nurse if mother has died.
 if whale mother-id = nobody [stop]
 
 RUN-MONITOR 1 "...is nursing."
 let kgs (desired-food-kgs * food-percent-from-milk)                                       ; amount of food to be acquired from nursing
 let mom-cond (([body-mass] of whale mother-id)   / ([expected-mass] of whale mother-id))  ; condition of mother based on ratio of actual to desired mass
 
 ; If mother is reasonable healthy, then nurse -- possibly in decreased amounts of mother is underweight 
  if  (mom-cond > LACTATION-CEASE) [                                 ; Check if mother is too underweight to nurse at all
    if (mom-cond <= LACTATION-DECREASE) [                            ; Check if mother is enough underweight to decrease milk production
        set kgs (kgs * (mom-cond - LACTATION-CEASE) * 10);           ;  ... decrease amount nursing whale receives based on mother's decreased health.
    ]                                                                ;  Note that LACTATION-DECREASE - LACTATION-CEASE <= .1
    ask whale mother-id [set milk-kgs-produced (milk-kgs-produced + kgs)]                  ; Update mother to indicate she has produced milk
    consume-food kgs MILK-KCAL-PER-GRAM MILK-TO-ENERGY-EFFICIENCY                          ; Update self to indicate food consumed
 ]
end


; ====================== START OF EATING AND HUNTING ==============

; CONTEXT: One whale (should be a group leader)
; Use statistical models to determine prey and hunting success in this vicinity, and share the food around the group.
; This procedure assumes there is available food near this patch. The whales do not move. The leader of the group
; assigns nutrition value to all the whales in the group based on the value of the prey.
to hunt
  RUN-MONITOR 1 " is deciding to hunt"
  let effective-group-size sum [effective-size] of group      ; number of whales in the group counting toward the hunt   
  let all-prey (get-prey-list effective-group-size)           ; lists every individual prey catchable, by type
  share-the-food all-prey
  
  ; PREY IN THE AREA ARE NOW WARNED ABOUT THE HUNTING FOR SOME SPECIFIED TIME
  ask patch-here [set prey-hiding hiding-time]
end

; CONTEXT: One whale (should be a group leader)
; Reports a list of all the prey in the patch that were encountered and potentially successfully hunted
; The list has one entry for each individual, giving the prey type of that individual
; ** This is a helper procedure for hunt
to-report get-prey-list [effective-group-size]
  ; FOR EACH TYPE OF PREY -- Determine the number that can be caught and potentially eaten 
  ; show (word "is getting a prey-list for this group, of size " effective-group-size)
  let result (list)
  let prey-type 0
  while [prey-type < num-prey-types][
    let class-type 0
    while [class-type < (item NUM-CLASSES# (item prey-type prey-data )) ] [
      ; The patch-here evaluates the following report to return an expected number of prey killed of the given type.
      let avg-prey-encountered (expected-prey-encounters prey-type class-type effective-group-size)    
      
      ; use the expected number to compute a encountered around a normal distribution
      let num-can-catch random-normal avg-prey-encountered (avg-prey-encountered / 5)    
      let frac remainder num-can-catch 1                                    ; store the fractional part
      set num-can-catch int (num-can-catch)                                 ; get rid of fractional part
      if (random-float 1.0) < frac [set num-can-catch (num-can-catch + 1)]  ; add one back with probability frac
      
      repeat num-can-catch [set result (lput prey-type result)]    ; Adds n individuals to the list of catchable prey where n=num-can-catch   
      set class-type (class-type + 1)
    ]
    set prey-type (prey-type + 1)
  ]
  
  ; randomize the all-prey list so that we take prey out in random order until whales are sated
  report (shuffle result) 
end



; CONTEXT: One whale (should be a group leader)
; Is passed a list of prey that were potentially successfully hunted. These prey are shared among all members of 
; the group, proportionally to their level of hunger. Each consumed prey lowers the population of that prey and 
; provides food for whales.
; ** This is a helper procedure for hunt
to share-the-food [all-prey]
  RUN-MONITOR 1 (word "is sharing a prey-list " all-prey)
  let the-group sort group                            ; An ordered list of the whales in this group.
  let desired-food map [desired-food-kgs] the-group   ; amount of food each whale wants, with whales ordered the same as in the-group
  let eaten-food map [0.0] the-group                  ; amount of food each whale has eaten, with whales ordered the same as in the-group
  let total-desired sum desired-food

  ; CONTINUE UNTIL NO MORE PREY OR ALL WHALES ARE SATED
  ; Traverse through each item in the all-prey list of prey, and share around the group proportional to the hunger level of each member.
  ; Stop when no more whales desire food
  foreach all-prey [
     let next-prey-type ?                             ; The current prey item from the list 
      
     ; reflect that this prey was killed, and is no longer in the world
     ask patch-here [
        let temp (item next-prey-type prey-counts)
        set prey-counts (replace-item next-prey-type prey-counts (temp - 1))
     ]
     
     ; Share the food among the whales in the group, each whale getting an amoung proportional to its hunger 
     let kg-killed current-mass next-prey-type 
     let kg-available kg-killed                     ; might be changed later to reflect not all calories get eaten
     
     ; Give each whale the smaller amount between what is available as their share and the amount they actually want
     let w 0                                                    ; loop through each whale
     while [w < length the-group][
        let my-want (item w desired-food)                       ; get this whales desires from the list
        let my-share (my-want / total-desired ) * kg-available  ; compute proportional share
        if (my-share > my-want) [set my-share my-want]          ; don't eat more than desired
        set desired-food (replace-item w desired-food (my-want - my-share))      ; decrease the amount this whale wants...
        let previously-eaten (item w eaten-food)
        set eaten-food (replace-item w eaten-food (previously-eaten + my-share))  ; and increase amount eaten
        consume-food my-share (item KCAL-PER-GRAM# (item next-prey-type hunting-data)) PREY-TO-ENERGY-EFFICIENCY
        set w w + 1                                             ; go on to the next whale
     ]
     set total-desired (total-desired - kg-available)
     if total-desired <= 0 [stop]                               ; don't continue to kill items from the preylist if there are no whales in the group who still desire food
  ]  
end
; ====================== END OF EATING AND HUNTING ==============


; CONTEXT: One whale (should be a group leader)
; Strategies (local optimization and past memory) to locate food
; This procedure assumes that the whales will move, and will not be hunting in the current patch
to seek-food
  RUN-MONITOR 1  " is seeking food"
  let travel-radius (whale-speed / kmpp)                                       ; distance in patches a whale can travel in one hour
  let group-size (sum [effective-size] of group)
  let best-nearby max-one-of other water-patches-within travel-radius [food-rating group-size]  ; patch in that radius with the most food
  
  ;If there is a patch within one time-unit of travel with enough food, then travel to that patch. Otherwise seek a more global solution.
  ;Note that if the whale was on long distance travel mode when it found local food, then the travel destination and path should be cleared.
  ifelse [food-rating group-size] of best-nearby >= 1 [ 
     RUN-MONITOR 1 (word " has decided to hunt local food and is clearing path. Destination was " destination)
     set destination nobody
     set current-path nobody

     move-to best-nearby                   ; *** MAY NEED TO FIX THIS LATER AS THIS MAY HAVE WHALES JUMPING OVER LAND.
     ask group [move-to best-nearby] 
   ][                   
     travel 
   ]
end

; CONTEXT: One whale (should be a group leader)
to socialize

end

;; ===================================== NON-HUNTING TRAVEL PROCEDURES =====================================
; CONTEXT: One whale (should be a group leader)
; This procedure follows a precomputed path to the desired destination. 
; The path is a list of patches stored in whale variable current-path. If that variable does not have a path, the path is computed.
to travel
   RUN-MONITOR 1  (word "is traveling, starting at " patch-here)
   if destination = nobody [
     choose-destination
     RUN-MONITOR 1 (word " chose destination " destination)
   ]
   ; If no path exists, compute a path from the current patch to the destination.
   if current-path = nobody [
     set current-path path-from-patch-to-patch patch-here destination
     RUN-MONITOR 2 (word " found the following path: " current-path)
   ]
   toward-destination
end

to choose-destination
    set destination one-of best-hunting-grounds      ; The whale's current destination -- initially any of the best hunting grounds.
end

; Follow the path of patches stored in current-path for whatever distance a whale can travel in one time step.
; Assumes that current-path is not empty, and that it starts with the whale's current patch or an adjacent patch.
to toward-destination
   let patches-can-travel (whale-speed / kmpp)      ; distance (in patch units) a whale can travel in one time step
   let group-size (sum [effective-size] of group)   ; ** CAN BE USED TO DECIDE WHETHER TO PAUSE AND HUNT LOCALLY **
   
   RUN-MONITOR 1 (word " moving toward " destination " a distance of " patches-can-travel " patches.")
   
   ; Whale should now keep moving until the total movement is equal to its maximum hourly travel distance.
   ; *** LATER ADD A WAY TO BREAK OUT OF THIS LOOP IF FODO IS FOUND -- ALSO A PROBABILISTIC VARIATION FROM THE ROUTE.
   while [patches-can-travel > 0 and length current-path > 0][
      ; *** FOR THE FOLLOWING TWO LINES... MIGHT BE MORE EFFICIENT TO KEEP A POINTER INTO THIS LIST AND JUST MOVE ALONG THE POINTER RATHER 
      ; THAN ACTUALLY REMOVING THE ITEM FROM THE LIST.  KEEP IT A FIXED SIZED ARRAY RATHER THAN A DYNAMIC QUEUE--DEPENDING ON HOW IT IS IMPLEMENTED.
      let next-patch first current-path                               ; Get the next patch of the current path
      set current-path but-first current-path                         ; Update the current-path to remove one patch
      let dist min (list (distance next-patch) patches-can-travel)    ; Determine how far the whale can travel
      face next-patch
      fd dist
      set patches-can-travel (patches-can-travel - dist)
   ]
   
   if patch-here = destination [
     RUN-MONITOR 1 (word " reached destination " destination)
     set current-path nobody 
     set destination nobody
   ]
   
   ; All other whales in this leader's group should end on the same patch with the same heading
   let dest patch-here
   ask other group [
     move-to dest 
     set heading [heading] of myself
   ]

end

;; ===================================== NON-MOVING WHALE BEHAVIOR PROCEDURES =====================================
; CONTEXT: One whale (should be a group leader)
to rest
  RUN-MONITOR 1 " is resting"
  ask group [set last-rest -1]                 ; whales are resting. At the end of this time step, it will be set to 0 time units since they last rested. 
end

; ======================WHALE LIFE DYNAMICS
;CONTEXT:  One whale
; This updates the whale's information based on the consumption of the given amount of food (kg-consumed) with the given caloric value (kcal-per-gram)
; and a whale's efficiency at converting that food
to consume-food [kgs kcal-per-gram efficiency]
   set gut-fill (gut-fill + kgs)
   set kcals-consumed (kcals-consumed + kgs * kcal-per-gram * 1000 * efficiency)
end
; 
;; CONTEXT: A whale with female? and pregnant?
;to birth-into-group
;end
;
;; CONTEXT: A whale with female? and pregnant?
;to baby-death
;end
;
;; CONTEXT: A whale with female? and not pregnant?
;to mate
;end

; CONTEXT: An adult whale 
; Handles fision and fusion of whale groups
to individual-group-dynamics
  if not female? and with-older-brother? [leave-and-form]
  if female? and with-calf? and with-mother? [leave-with-calf-and-form]
end

; CONTEXT: An adult whale 
to leave-and-form
end

; CONTEXT: An adult female whale 
to leave-with-calf-and-form
end

to fision
end

to fusion
end

; CONTEXT: One Whale 
; Updates a whale's mass -- adds to mass based on food consumed, but subtract based on metabolism
to update-mass
   ; caldiff represents the difference between what was consumed and the daily metabolic requirement
   ; caldiff can be positive or negative. If negative, it results in weight loss. If positive, then in weight gain. If within 0.01, then no change.
   let caldiff (kcals-consumed - compute-FMR)
   
   ; Caldiff can be negative or positive. If it's absolute value is close to 0, then no weight change.
   ; If whale ate more or than needed, convert extra calories to mass. If less then needed, whale loses weight. Otherwise, no change.
   if (abs caldiff > 0.01) [
     set body-mass (body-mass + ((caldiff / WHALE-KCAL-PER-KG) * ENERGY-TO-MASS-EFFICIENCY))
   ]
end

; ---------------------WHALE REPORTERS -------------
to-report desired-food-kgs
  let A 25.0                            ;
  let B -25.0                           ;   Constants for logistic satiation curve
  let target-mass expected-mass   
  let x exp( A + B * body-mass / target-mass )       ; Factors in both gut size and desired mass 
  let cFMR compute-FMR * PREY-TO-ENERGY-EFFICIENCY / WHALE-KCAL-PER-KG;
  let kg cFMR + ((GUT-MASS-PERCENT * target-mass) - cFMR) * (x / (1 + x));
  if is-nursing? [set kg kg - food-percent-from-milk]
  report kg
end

; Reports true if this whale would like to eat more.
; *** FOR NOW, this returns true if and only if the amount of kgs consumed (the gut-fill) is less than the desired-food.
to-report is-hungry?
   report (gut-fill < desired-food-kgs)   
end

; REPORTER FOR A LEADER OF A WHALE GROUP
; Reports true if any whale in the group of this leader whale is hungry
to-report group-hungry?
   report (any? group with [is-hungry?])
end

; This reporter is called by a group leader.  It reports true if, for any members of this leader's group, the number
; of time periods since the last rest is beyond the threshold for when they get tired.
to-report group-tired?
   report any? group with [last-rest > tired-threshold]
end

; Reporter for a given whale, determines if its currently location has enough food to justify hunting
to-report food-present?
   let group-size (count group)
   report [food-rating group-size] of patch-here > MIN-HUNTING-K
end
   
to-report meeting?
   report true
end

; CONTEXT: A whale with female? 
; returns true if in the same group as its calf 
to-report with-calf?
   report false
end

; CONTEXT: Any whale
; returns true if in the same group as its mother 
to-report with-mother?
   report false
end

; CONTEXT: Any whale (called only for males) 
; returns true if in the same group as an older brother 
to-report with-older-brother?
   report false
end

; CONTEXT: A whale with female? and pregnant?
to-report pregnant-mass-loss?
   report false
end
   
; CONTEXT: A whale with female? and pregnant?
to-report time-to-birth?
   report false
end

to-report better-to-split?
   report false
end

to-report time-to-join?
   report false
end

to-report time-to-conceive?
  report false
end

to-report starving?
  report false
end

; reports the effective size of a single whale -- what that whale contributes to a hunt, as a proportion of what a healthy adult would contribute
; CONTEXT: ONE WHALE
to-report effective-size
    if age-years < HUNT-AGE-MIN [ report 0 ]    ; whales that are too young do not help in a hunt
      
    let es 1.0;     ; the default and initial value of effective size
    let mass-percent (body-mass / expected-mass)
 
    if (mass-percent < STARVE-END-PERCENT) [report 0]  ; should be dead?

     ; scale from youngest hunting age to oldest hunting age
    if age-years < HUNT-AGE-MAX [
       set es es * (age-years - HUNT-AGE-MIN + age-days / 365)/(HUNT-AGE-MAX - HUNT-AGE-MIN)
    ]


    ; Scale EFFECTIVE-SIZE-MAX (at STARVE-BEGIN-PERCENT) to 0 (at STARVE-END-PERCENT)
    ifelse (mass-percent < STARVE-BEGIN-PERCENT) [
       set es es * (1.0 + EFFECTIVE-SIZE-MAX / (STARVE-BEGIN-PERCENT - STARVE-END-PERCENT) * (STARVE-BEGIN-PERCENT - mass-percent))
    ][
      ifelse (mass-percent < 1)[
         set es es * (1.0 + (EFFECTIVE-SIZE-MAX - 1.0) / (1.0 - STARVE-BEGIN-PERCENT) * (1.0 - es));
      ][
         report 1
      ]
    ] 

    if (es < 0) [ Show "Whale has negative effective size. Something is very wrong." ]

    report es
end

; reports what the mass of the whale ought to be, based on its age, pregnancy status, gender.
; CONTEXT: one whales    
to-report expected-mass
   let totalDays age-years * 365 + age-days
   ifelse female? [
      let maxmass FEMALE-MAX-MASS - (FEMALE-MAX-MASS - BIRTHMASS) * exp (- FEMALE-VON-BERT * totalDays)
      if days-to-birth > 0 [set maxmass maxmass + (fetal-mass (DAYSPREGNANCY - days-to-birth)) * (1 + PREGNANCY-WEIGHT-GAIN + PREGNANCY-TISSUE-MASS)]
      report maxmass
   ][
      report MALE-MAX-MASS - (MALE-MAX-MASS - BIRTHMASS) * exp (- MALE-VON-BERT * totalDays)
   ]              
end

; n is a number of days since conception.
to-report fetal-mass [ n ]
  report  BIRTHMASS / (1 +  exp (-15 * (n / DAYSPREGNANCY - .68)));
end


to-report fetal-growth [ n ]
  report fetal-mass n - fetal-mass (n - 1)
end

; Daily metabolic requirement
to-report compute-FMR
  let fmr 0
  let percent-actual-mass body-mass / expected-mass
  ifelse (percent-actual-mass < STARVE-BEGIN-PERCENT) [
    ;Scaling factor, whale gets less active as it starves (lower percent of active mass)
    let less-active-correction-factor (STARVE-BEGIN-PERCENT - percent-actual-mass) * 100 * 13.5;
    if (less-active-correction-factor > FMR-CONSTANT) [
       set less-active-correction-factor FMR-CONSTANT
    ]
    set fmr ((FMR-CONSTANT - less-active-correction-factor) * (body-mass ^ FMR-EXPONENT))
  ][
    set fmr (FMR-CONSTANT * (body-mass ^ FMR-EXPONENT));
  ]
  if female? [
    if milk-kgs-produced > 0 [set fmr (fmr + milk-kgs-produced * MILK-KCAL-PER-GRAM * 1000 / ENERGY-TO-MILK-EFFICIENCY) ]
    if days-to-birth > 0 [set fmr (fmr + fetal-growth (DAYSPREGNANCY - days-to-birth) * (1 + PREGNANCY-TISSUE-MASS) * WHALE-KCAL-PER-KG / ENERGY-TO-FETUS-EFFICIENCY) ]
  ]

  report fmr
end

to-report food-percent-from-milk
  if age-years > 2 [report 0]
  ; ** IF NO LIVING PARENT, SHOULD REPORT 0 ***
  let n (age-years * 365) + age-days
  let A 6.1            ;   // Constants for logistic satiation curve
  let B -12.0;
  let C 6.0;
  let z exp (A + B * ( .01 * n / C));
  report z / (1 + z);
end


; CONTEXT: One Whale
; This determines if a whale has starved to death. Reports a boolean that is true if the whale has starved
to-report starved-to-death? 
  report (body-mass < STARVE-END-PERCENT * expected-mass)
end



; CONTEXT: One Whale
; This uses probabilistic values to determine if a given whale should die based on its
; age, gender, and tables giving the probability of survival.
to-report died-probabilistically?
  let pSurv 0
  let pDie 1
  ifelse female? [set pSurv item age-years FEMALE-SURV] [set pSurv item age-years MALE-SURV]
  
;  if (age-years < JUVENILE_MAX) [
;    if (USE_DENSITY_DEPENDENCE) [
;      pSurv = pSurv * (1 - model.getLiveWhales() / JUVENILE-K);
;    ]  
;    if (pSurv < 0) pSurv = 0;
;  ]
  
  ; Check for neonate, first six months of the year use pNeonateSurv
  ; The rest of the months of the year, use what is in the file
  
  ifelse (age-years = 0) [
    if (age-days < 182) [      
      set pSurv P-NEONATE-SURV;
    ]  
    set pDie (1 - pSurv ^ (1.0 / ( 365 / 2.0 )));
  ][
  set pDie (1 - pSurv ^ (1.0 / 365));
  ]
  
  let d random-float 1.0
  
  ifelse (d < pDie or pSurv  < 0.01) [
    report true
  ][
  report false
  ]
end

;to plot-masses
;  set-current-plot "Whale Body Mass Groups"
;  set-current-plot-pen "<80%"
;  plot count whales with [body-mass / expected-mass < .8] / count whales
;  set-current-plot-pen "80-90%"  
;  plot count whales with [body-mass / expected-mass >= .8 and body-mass / expected-mass < .9] / count whales
;  set-current-plot-pen ">90%"
;  plot count whales with [body-mass / expected-mass >= .9] / count whales
;  
;end
@#$#@#$#@
GRAPHICS-WINDOW
238
10
1289
678
-1
-1
1.0
1
10
1
1
1
0
0
0
1
0
1040
0
636
0
0
1
ticks
30.0

BUTTON
4
10
70
43
NIL
setup\n
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
4
48
61
93
NIL
hours
17
1
11

MONITOR
66
49
123
94
NIL
days\n
17
1
11

MONITOR
6
97
63
142
NIL
years
17
1
11

BUTTON
77
11
145
44
run
move
T
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

SWITCH
2
588
128
621
track-whales
track-whales
0
1
-1000

MONITOR
4
146
61
191
Births
whales-born
17
1
11

MONITOR
66
145
160
190
Newborn Deaths
 whales-died-at-birth\n
17
1
11

MONITOR
3
195
95
240
Conceptions
  successful-mating\n
17
1
11

MONITOR
6
246
112
291
Starvation Deaths
whales-starved\n
17
1
11

SLIDER
1
294
218
327
days-per-hunting-season
days-per-hunting-season
10
100
98
2
1
NIL
HORIZONTAL

BUTTON
3
546
77
579
NIL
redraw\n
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
1
629
217
674
map-mode
map-mode
"shore distance" "shore distance w/ graph" "prey density" "hunting regions" "hunting regions w/ graph" "voronoi" "voronoi w/ graph"
0

@#$#@#$#@
## WHAT IS IT?

This section could give a general understanding of what the model is trying to show or explain.

## HOW IT WORKS

This section could explain what rules the agents use to create the overall behavior of the model.

## HOW TO USE IT

This section could explain how to use the model, including a description of each of the items in the interface tab.

## THINGS TO NOTICE

This section could give some ideas of things for the user to notice while running the model.

## THINGS TO TRY

This section could give some ideas of things for the user to try to do (move sliders, switches, etc.) with the model.

## EXTENDING THE MODEL

This section could give some ideas of things to add or change in the procedures tab to make the model more complicated, detailed, accurate, etc.

## NETLOGO FEATURES

This section could point out any especially interesting or unusual features of NetLogo that the model makes use of, particularly in the Procedures tab.  It might also point out places where workarounds were needed because of missing features.

## RELATED MODELS

This section could give the names of models in the NetLogo Models Library or elsewhere which are of related interest.

## CREDITS AND REFERENCES

This section could contain a reference to the model's URL on the web if it has one, as well as any other necessary credits or references.
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
0
Rectangle -7500403 true true 151 225 180 285
Rectangle -7500403 true true 47 225 75 285
Rectangle -7500403 true true 15 75 210 225
Circle -7500403 true true 135 75 150
Circle -16777216 true false 165 76 116

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
NetLogo 5.0.4
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
default
0.0
-0.2 0 1.0 0.0
0.0 1 1.0 0.0
0.2 0 1.0 0.0
link direction
true
0
Line -7500403 true 150 150 90 180
Line -7500403 true 150 150 210 180

@#$#@#$#@
0
@#$#@#$#@
