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
  ; AGE OF WHALE.  Used to compute hunting efficiency, expected mass, reproductive success, etc.
  age-years   
  age-days

  ; GENERIC INSTANCE VARIABLES RELATED TO PHYSIOLOGY
  is-nursing?                      ; boolean for a young whale that is still dependent on the milk of its mother
  body-mass                        ; current mass of the whale. can be compared with expected mass for a whale of this age.
  gut-fill                         ; kgs consumed but not yet digested. whales can only eat a certain amount, and then must digest.
  kcals-consumed                   ; energy intake durrent the current feeding cycle
  milk-kgs-produced                ; used to compute cost of lactation for females
  
  ; INSTANCE VARIABLES FOR FEMALE WHALES, RELATED TO PREGNANCY AND LACTATION
  days-to-birth                    ; for pregnant whales, how many days until birth of whale
  pregnancy-cost                   ; metabolic cost of being pregnant
  conception-day                   ; day that a female concieved. 
  female? pregnant? lactating?     ; boolean variables for female whales
  had-male-child?

  ; INSTANCE VARIABLES SOCIAL DYNAMICS
  mother-id                        ; unique ID of this whale's mother, used for matrilineal lines
  is-leader?                       ; set to true if this whale is a group leader
  group                            ; a list of whales in this whale's current group -- only used if is-leader is true.
  leader                           ; the whale who leads this group -- only used if is-leader? is false.
  ; whales-met                       ; *** FOR FUTRE *** a list of whales with which this whale has associated

  ; INSTANCE VARIABLES USED FOR TRAVEL AND HOURLY DESCISION MAKING
  last-rest                       ; If >0, this is the time steps since last rest. If <0, then the whale is currently resting and must rest for |last-rest| hours.
  destination                     ; a whale's current destination, given as a patch.   
  current-path                    ; If the whale is in travel mode (long distance) then this contains a list of patches giving a path to the whale's destination.
  
  ; Data on whale's past hunting experiences.    See memory.nls for more complete description of the information and format.
  memory
]

globals [
  ; General global variables used for the simulation
  hours days years             ; keeps track of the current time.  one tick = one hour.
  
  leaders                      ; a list of whales that are current group leaders (and decision makers)
      
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
  
  ; Set levels of reporting and user messages
  set SETUP-MONITOR-LEVEL 2
  set DEBUG-MONITOR-LEVEL 2
  set RUN-MONITOR-LEVEL 2     ; 1 = minimal file output   2 = file output of hunting events and daily decisions    3 = file output of detailed decisions

  ; Some globals that determine how the simulation is run
  set INITIAL-NUMBER-WHALES 180
  set USE-DENSITY-DEPENDENCE? true
  set SCALE-REPRO-K-AGES-10-12? true
  set MIN-HUNTING-K  1.5
    
  ; Initialize statistics variables
  set hours 0
  set days 0
  set years 0
  set successful-mating 0
  set whales-born 0
  set whales-died-at-birth 0
  set whales-starved 0
  set other-deaths 0
    
  ; Initialize the physical world and the underlying geographic data structures 
  load-map                  ; Get map data (which patches are are land, coast, and water) from an image and associated text file.
  set-distance-in           ; For each water patch, find out how far it is from the coast, or from open water.
  compute-vd                ; Identify islands. For each water patch, compute the nearest land patch -- this implicitly gives the Voronoi diagram.
  make-graph                ; Create a graph of nodes (including special anchor nodes) to be used for long distance whale movement
  all-anchors-SSSP          ; Compute the shortest path from all graph nodes to all anchor nodes, and store the shortest-path info in the nodes. 

  ; Load data from data files for the prey and the whales
  init-prey  
  init-whales
   
  let file-name (word "Testrun-" (substring date-and-time 16 27) "-" (substring date-and-time 0 8))
  file-open file-name
  RUN-MONITOR 0 (word "Whale test run on " date-and-time " with " INITIAL-NUMBER-WHALES " whales.") 

  ask water [set visited? -1]    ;; **** COMMENT HERE... AND ON THE USE OF VISITED... AND REVISIT WATER-PATCHES-WITHIN in MAP ********
  
  reset-ticks
end

to save-current-world
  export-world "se-alaska-data.csv"
end

;; =========================================================================================================
; Initialization of the population of whales
; In v 1.0, whales ages, genders, etc. are random, as are the matrilineal lines
; This should be called only once during the initialization of the simulation.
to init-whales
  SETUP-MONITOR 0 "Initializing whales..."
  init-whale-globals                               ; Global constants having to do with whales                
  whale-genesis                                    ; Create all the initial whales at start of simulation
  create-whale-hunting-groups                      ; Create whale groups with leaders and matrilineal lines
  initialize-random-whale-memories 5           ; Give leaders of each group an initial artificial memory of past hunting
  
  ; Select whale groups to track with pens, based on the tracking-mode chooser.
  if tracking-mode = "all groups" [ask leaders [pen-down]]
  if tracking-mode = "one group" [ask one-of leaders [pen-down]]
end

; Procedure to actually create the starting number of whales and initialize the whales-own variables for each.
; None of the whales are pregnant or lactating, and all have a random age from 0 to 24. 
; This should be called only once, by init-whales, during the initialization of the simulation.
to whale-genesis

  ; initialize a random population of whales, 50% female, aged 2 to 24, with no pregnant or lactating females
  create-whales INITIAL-NUMBER-WHALES [
    ; set whales-met (list )
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
end


; Procedure to take the existing initial group of whales and divide them into (random) hunting groups with (random) matrilineal lines
; and a group leader for each group.
; This should be called only once, by init-whales, during the initialization of the simulation.
to create-whale-hunting-groups
  ; Assign to whales a random older female as its mother. Every whale should have a mother-id except the oldest females, which are the matriarchs.
  let matriarch-age [age-years] of max-one-of whales with [female?] [age-years]  ; find age of the oldest female
  ask whales [                                                                   ; ask other whales to set their mother-id (unless too old to be offspring of matriach)
    ifelse (age-years + 5) < matriarch-age [                           ; if more than 5 years younger than the matriarch, assign a random older female whale as mother
      set mother-id [who] of one-of other whales with [female? and age-years > 5 + [age-years] of myself]
    ][
      set mother-id -1                                                 ; Whales too old to be the offspring of the matriarch no longer have a living mother.
    ]
  ]
  
  ; Pick group leaders. To create average group sizes of 3.5, we let 1/3.5 of the whales be leaders.  Leaders should be at least 10 years old.
  ; Leaders will be initialized at some random inland patch.  Other whales assigned to their group will join them.
  ask n-of (INITIAL-NUMBER-WHALES / 3.5) whales with [age-years >= 10] [
    set is-leader? true                                               ; identify this whale as a leader who will be making group decisions
    set group (turtle-set self)                                       ; put each leader in her own group
    move-to one-of water with [shore-dist < 30 and shore-dist > -1]   ; move to a random patch which isn't "too far out"
  ]
  ask whales with [not is-leader?] [join-random-group]                ; put all whales in some group
  balance-groups                                                      ; there should be variance among group sizes, but don't allow groups of just 1 adult
  set leaders whales with [is-leader?]                                ; Create a list of group leaders for later efficiency
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
   move-whales                      ; Handles all whale behavior
   update-prey                      ; Updates all prey hiding 
   increment-clock
   if (hours = 0) [
     ask whales [daily-step]        ; Daily metabolism -- whales digest food, thus emptying their gut.
     ; NEW PROCEDURE.  CHECKS IF THIS IS THE FIRST DAY OF A NEW SEASON FOR ANY PREY.  IF SO... gather-and-count AND redistribute
     let distributable-prey first-day-of-season? days
     foreach distributable-prey [
       gather-and-count ?
       seasonal-distribution ? days
     ]
   ]        
end

; CONTEXT: Observer
; Update the global clocks.  Each tick represents 1 hour == the temporal scale of this simulation.
to increment-clock
   set hours hours + 1             ; Add a tick's worth of hours
   if hours = 24 [                 ; If past 23 hours, start a new day.  (Hours are numbered 0..23)
     set hours 0
     set days days + 1
     ; plot-masses
   ]
   if days = 365 [                 ; If past 364 days, start a new year.  (Days are numbered 0..364. There are no leap years modeled.)              
     set days 0
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
   RUN-MONITOR 1 "Group leader making a movement decision for group."
   ask group [update-rest]      ; increment counters indicating length of time since last rest
   ifelse group-tired? [        ; Fatigue takes first precedent in decision making
     ask group [rest]
   ][                           ; ----- Whales are not tired...
     ifelse group-hungry? [     ; If any whales (in the group) need energy, they will try to get food
       seek-food
     ][                         ; ----- Whales are not tired or hungry...
       ifelse meeting?          ; If meeting another group, then socialize, otherwise travel
          [socialize]           
          [travel]             
     ]
  ]
end

; CONTEXT: One whale
; This should be called once per day - that is, whenever hours=0, or equivalently when ticks mod 24 = 0.
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

; Pregnancy of female whales is entirely probabilistic, and does not involve encounters with male whales
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
  
  ; PREY IN THE AREA ARE NOW "WARNED" ABOUT THE HUNTING FOR SOME SPECIFIED TIME
  ask water in-radius hiding-radius [set prey-hiding hiding-time]
end

; report the number of kgs a whale in its given group would expect to consume if it hunted for one hour at the given patch
to-report kgs-to-be-gained-here [ effective-group-size ]
  let all-prey get-prey-list effective-group-size
  let total-mass-to-share 0
  foreach all-prey [set total-mass-to-share total-mass-to-share + current-mass (first ?) (last ?)] 
  
  ; The following assumes the proportion of the total-mass-to-share correlates with the effective size of the individuals in the group
  report total-mass-to-share / effective-group-size
end


; CONTEXT: One whale (should be a group leader)
; Reports a list of all the prey in the patch that were encountered and potentially successfully hunted.  
; Each item in the list represents an individual prey as a pair--that is, a length two list: (prey-type, class-type).
; Again, the list has one entry for each individual, giving the prey type and class type of that individual
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
      
      ; use the expected number to compute actual # encountered around a normal distribution
      let num-can-catch random-normal avg-prey-encountered (avg-prey-encountered / 5)    
      let frac remainder num-can-catch 1                                    ; store the fractional part
      set num-can-catch int (num-can-catch)                                 ; get rid of fractional part
      if (random-float 1.0) < frac [set num-can-catch (num-can-catch + 1)]  ; add one back with probability frac
      
      repeat num-can-catch [set result (lput (list prey-type class-type) result)]    ; Adds n individuals to the list of catchable prey where n=num-can-catch   
      set class-type (class-type + 1)
    ]
    set prey-type (prey-type + 1)
  ]
  
  ; randomize the all-prey list so that we take prey out in random order until whales are sated
  report (shuffle result) 
end



; CONTEXT: One whale (should be a group leader)
; The whale is passed a list of prey that were potentially successfully hunted. 
; Each element in that list is a (prey-type, class-type) pair representing a single prey successfully hunted. 
; These prey are shared among all members of the group, proportionally to their level of hunger. 
; Each consumed prey lowers the population of that prey on the given patch, and provides food for whales.
; ** This is a helper procedure for hunt
to share-the-food [all-prey]
  RUN-MONITOR 1 (word "is sharing a prey-list " all-prey)
  
  ; The following three lists represent the group of whales, the food they desire, and the food they have eaten, with each list having the same ordering of whales
  let the-group sort group                            ; An ordered list of the whales in this group.
  let desired-food map [desired-food-kgs] the-group   ; amount of food each whale wants, with whales ordered the same as in the-group
  let eaten-food map [0.0] the-group                  ; amount of food each whale has eaten, with whales ordered the same as in the-group
  
  let total-desired sum desired-food                  ; This is the total amount of 

  ; CONTINUE UNTIL NO MORE PREY OR ALL WHALES ARE SATED
  ; Traverse through each item in the all-prey list of prey, and share around the group proportional to the hunger level of each member.
  ; Stop when no more whales desire food
  foreach all-prey [
     let next-prey-type (first ?)                      ; The prey-type of the current prey item from the list 
     let next-class-type (last ?)                      ; The class-type of the current prey item from the list 
      
     ; reflect that this prey was killed, and is no longer in the world by decrement the count on this patch of the number of prey of the given type and class
     ask patch-here [
        let temp-list (item next-prey-type prey-counts)
        let temp-item (item next-class-type temp-list)
        set temp-list (replace-item next-class-type temp-list (temp-item - 1))
        set prey-counts (replace-item next-prey-type prey-counts temp-list)
     ]
     
     ; Share the food among the whales in the group, each whale getting an amount proportional to its hunger 
     ; kg-killed is the mass of the prey actually killed
     ; kg-available is a proportion of kg-killed that is actually eaten
     let kg-killed (current-mass next-prey-type next-class-type)
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
        consume-food my-share (item KCAL-PER-GRAM# (item next-class-type (item next-prey-type hunting-data))) PREY-TO-ENERGY-EFFICIENCY
        set w w + 1                                             ; go on to the next whale
     ]
     set total-desired (total-desired - kg-available)
     if total-desired <= 0 [stop]                               ; don't continue to kill items from the preylist if there are no whales in the group who still desire food
  ]  
  
  ; Update the memory of each whale in this group (represented by ?1) to show how many kgs of food it consumed (represented as ?2) from 1 hour of hunting in this hunting region
  (foreach the-group eaten-food [ask ?1 [update-whale-memory-here-and-now ?2]])
end


; ====================== END OF EATING AND HUNTING ==============


; CONTEXT: One whale (should be a group leader)
; Strategies (local optimization and past memory) to locate food
; This procedure assumes that the whales will move, and will not be hunting in the current patch
; A whale will choose either to hunt in its current region, or to start traveling to another region.
to seek-food
  RUN-MONITOR 1  " is seeking food and decided where to travel"
  let the-season hunting-season-of-day days                                    ; Get the current season number for accessing seasonal memory structure
  let travel-radius (whale-speed / kmpp)                                       ; Distance in patches a whale can travel in one hour
  let group-size (sum [effective-size] of group)                               ; The whale's group size, used to determine both hunting success and amount of sharing

  ; Estimate the whale's net weight gain (or loss if the number is negative) if it remains hunting nearby for the given number of days in evaluation-period.
  ; To compute the kgs of food consumed...
  ;  ... the estimated number of kgs a whale could get hourly if it hunts at the best spot within a 1 day radius is multiplied by the number of hours it will hunt
  let nearby water-patches-within travel-radius 0
  let best-nearby max-one-of nearby [kgs-to-be-gained-here group-size]     ; patch in that radius with the most food
  let hourly-gain [kgs-to-be-gained-here group-size] of best-nearby                                          ; hourly kgs expectation to be gained nearby
  let waking-hours (max-active-ratio * evaluation-period * 24)          ; total number of hours a whale is active (not resting) over the given number of days
  let max-hunting-hours (waking-hours * 0.5)                            ; *** Since hunting causes prey to hide, whales spent 50% of their time moving around locally, and 50% hunting        
  let kgs-gained (hourly-gain * max-hunting-hours)                      ; with the given number of hours to hunt, how many kgs of food will it get here.
  let kcals-lost (compute-FMR * evaluation-period)                      ; how many kcals will a whale expend in energy in that same amount of time...
  let kgs-lost  ((kcals-lost / WHALE-KCAL-PER-KG) * ENERGY-TO-MASS-EFFICIENCY)          ; ...converted to kgs.
  let net-gain (kgs-gained - kgs-lost)                                  ; expected result of a whale hunting locally               
  
  ; Keep track of the best possible hunting so far.  If the best-region is "nobody", it means the whale hunts locally.
  ; If the best-region stores the whale's current region, then the whale will start over again near the center of that region.
  let best-region nobody  
  let best-net-gain net-gain
  
  ; Continue to compare the best region considered so far with the estimated result of all the regions in the whale's memory for the current season.
  foreach (item the-season memory) [
    ; An item of the hunting memory for this season is a triple (r,h,w) where r is the region, h the number of hours hunted there during the same season of
    ; this or some prior year, and w the total mass actually consumed by this whale while hunting in the given region and season.
    ; Extract the three items from the whale memroy triple.
    let r first ?
    let h item 1 ?
    let w last ?
    ; Compute the estimated hourly consumption gain (kgs) from hunting in that region, and also the travel time (hours) to that region
    set hourly-gain (w / h)
    let travel-time (estimated-distance-to r / whale-speed)
    
    ; Find the max time that might be spent hunting. 
    ; The evaluation period is in days, and needs to be mutiplied by 24 to convert to hours.
    ; Actual hunting hours is the total number of hours times the ratio of active hours minus the long-distance time spent to travel to the region...
    ; ... and at the end multiply by .5 under the assumption that after whales hunt one spot, the prey there will be hiding and more short-distance travel
    ; will be needed. 
    ; *** AS ABOVE, THE 0.5 CONSTANT PERHAPS SHOULD CHANGE. IN FACT, IF WHALE'S REST AFTER EATING, THIS MIGHT BE BUILT INTO THE RESTING. HOWEVER
    ; WHALE TRACKING DATA SHOW LOTS OF LOCAL MOVEMENT ***
    set waking-hours (max-active-ratio * evaluation-period * 24 - travel-time) 
    set max-hunting-hours (waking-hours * 0.5)
    
    ; Given the number of hours actually hunting, find the total kgs expected to be consumed based on the whale's memory of prior hunting at the given region
    set kgs-gained hourly-gain * max-hunting-hours
    
    ; Compute the whale's metabolic weight loss over the given period.  Note that compute-FMR is daily expected metabolic cost in kcals, and evaluation-period is in days
    set kcals-lost (compute-FMR * evaluation-period)
    set kgs-lost  ((kcals-lost / WHALE-KCAL-PER-KG) * ENERGY-TO-MASS-EFFICIENCY)
    
    ; compare the net-gain with the best seen so far, and update accordingly.
    set net-gain kgs-gained - kgs-lost
    if net-gain > best-net-gain [
      set best-net-gain net-gain
      set best-region r
    ]
  ]
 
  ; Set the destination.  If best-region = nobody, then there was no long-distance memory that would be better in the longer term than hunting with a one-hour radius
  ifelse best-region = nobody [
    ; It is already determined that best-nearby is within one hour travel.
    ; The most computationally efficient is just to move the whale there -- though if the whale is being tracked, it might appear to move over land
     RUN-MONITOR 1 (" has decided to hunt local food and is clearing path.")
     set destination nobody
     set current-path (list )
     move-to best-nearby
     ask other group [move-to best-nearby]  
     if [visited?] of best-nearby < travel-radius / 2 [hunt]   ;  **** If travel time less than 1/2 hour, then travel and hunt in the same turn. MODIFY *****
  ][  
     RUN-MONITOR 1 (word " has decided to hunt in region " best-region " and is creating a path for that region.")
     set destination [patch-here] of (one-of nodes with [([id] of nearest-anchor) = best-region])  ; ** LIST OF ANCHORS WOULD MAKE THIS MORE EFFICIENT **
     set current-path sparse-path-to-patch destination
     travel 
   ]
   ask nearby [set visited? -1]           ; ******* PART OF THE USE OF visited? NEEDING COMMENTING **********
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
;     choose-destination
     RUN-MONITOR 1 (word " chose destination " destination)
   ]
   ; If no path exists, compute a path from the current patch to the destination.
   if length current-path = 0 [
     set current-path sparse-path-to-patch destination
     RUN-MONITOR 2 (word " found the following path: " current-path)
   ]
   toward-destination
end

;to choose-destination
;    set destination one-of best-hunting-grounds      ; The whale's current destination -- initially any of the best hunting grounds.
;end

; Follow the path of patches stored in current-path for whatever distance a whale can travel in one time step.
; Assumes that current-path is not empty, and that it starts with the whale's current patch or an adjacent patch.
to toward-destination
   let patches-can-travel (whale-speed / kmpp)      ; distance (in patch units) a whale can travel in one time step
   let group-size (sum [effective-size] of group)   ; ** CAN BE USED TO DECIDE WHETHER TO PAUSE AND HUNT LOCALLY **
   
   RUN-MONITOR 3 (word " moving toward " destination " a distance of " patches-can-travel " patches.")
   DEBUG-MONITOR 3 (word " moving toward " destination " along path " current-path)
   
   ; Whale should now keep moving until the total movement is equal to its maximum hourly travel distance.
   ; *** LATER ADD A WAY TO BREAK OUT OF THIS LOOP IF FODO IS FOUND -- ALSO A PROBABILISTIC VARIATION FROM THE ROUTE.
   while [patches-can-travel > 0 and length current-path > 0][
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
; CONTEXT: One whale
; Indicate the whale is resting.
; If last-rest is positive, set it to 0 or a negative number to indicate the whale is resting.
; |last-rest| is the number of additional ticks (after this one) that the whale needs to continue restign. 
to rest
  RUN-MONITOR 1 " is resting"
  if last-rest >= max-time-without-rest [set last-rest (1 - rest-time)]
  if last-rest > 0 [set last-rest 0]               
end

; CONTEXT: One whale.  
; Update last-rest counter indicating how long since it last rested.  If last-rest < 0, then the whale has just rested.
to update-rest
  set last-rest (last-rest + 1)
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
   report any? group with [last-rest >= max-time-without-rest or last-rest < 0]
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

; The percentage of time a whale spends active if it goes the max time without rest, and then rests the minimum time.
; It is dependent on the values of the sliders max-time-without-rest and rest-time
to-report max-active-ratio
  report (max-time-without-rest / (rest-time + max-time-without-rest))
end

;;;; Utility functions
to-report zip [ziplist]
  report reduce [(map [SENTENCE ?1 ?2] ?1 ?2)] ziplist
end

to-report flatten-once [multilist]
  report reduce [SENTENCE ?1 ?2] multilist
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

SLIDER
0
329
177
362
evaluation-period
evaluation-period
1
15
5
1
1
days
HORIZONTAL

SLIDER
-2
365
236
398
max-time-without-rest
max-time-without-rest
5
48
24
1
1
hours
HORIZONTAL

SLIDER
0
400
178
433
rest-time
rest-time
1
10
4
1
1
hours
HORIZONTAL

CHOOSER
1
581
139
626
tracking-mode
tracking-mode
"no whales" "one group" "all groups"
0

SLIDER
0
435
190
468
hiding-radius
hiding-radius
.5
10
5
.5
1
patches
HORIZONTAL

@#$#@#$#@
## WHAT IS IT?

This is a spatial individual-based model of killer whale life-cycle dynamics in southeast Alaska. The emphasis is on predation and whale movement, including whale physiology. However life-cycle dynamics also require modeling reproduction and mortality, with mortality based on physiology, which is modeled using metabolism and consumption (and thus predation success.)

Whales are modeled as individuals.  The temporal granularity is one tick = one hour.

Prey are not modeled as individuals; they are modeled as population counts at the spatial granularity level. The spatial granularity is patches of size approximately .6km x .6km. Thus each ~.36km2 patch has a population count for each age class of each prey species included in the model.  Successful hunting and consumption of prey in a spatial patch reduces the population count of that prey on that patch. Prey mortality due to non-whale factors, as well as reproduction (seasonal adjustments of age classes) is handled stochastically at a population-wide level.

(* Future enhancements will include whale group social dynamics such as fusion and fission events. *)


## HOW TO USE IT

Set the values of the sliders, or press the <default> button to reset them.
Press <setup> and get some coffee. It will take a few minutes to initialize the map and geography, read the prey files and load the data files then seasonally disburse the prey, and final create and initialize the whales including their memory.

TO IMPLEMENT: RANDOM vs PRECREATED SETUP

Press <run> and watch.

It is not recommended that you adjust the sliders during a run. Set the sliders, watch what happens, then adjust.  

COMMENT ON: OUTPUT FILES

## HOW IT WORKS: IMPORTANT CONSTANTS:

There are numerous program constants directly impacting a killer whale's hunting success, and thus the population dynamics of  both the predator (killer whales) and the prey populations. Some are given as sliders to make it easier for the user to experiment with different values. Some are defined as globals and initialized within the program, typically in the constants file.

Below is a list of important constants (globals and slider) arranged in groups. These can be changed in the code or with sliders.  They should all be set at the start of the run.

The sliders max-time-without-rest and rest-time both have hours as units, and collectively determine the maximum ratio of time that a whale is active, either traveling or hunting.  
•	The slider max-time-without-rest determines the maximum number of hours a whale can hunt or travel before it needs to rest.  
•	The slider rest-time determines the minimum number of hours a whale must rest before it can become active again.  
Thus (max-time-without-rest / (max-time-without-rest + rest-time)) is the maximum ratio of time a whale is active.

As soon as killer whales hunt in an area (for a single hour = 1 tick), prey in that area will go into hiding and become more difficult to hunt. There are three constants that determine the level of hiding. 
•	The slider hiding-radius determines over how large a spatial area prey will be aware of hunting whales and go into hiding. When whales hunt in a given patch, then all prey within hiding-radius of that patch will start hiding for some period of time, and thus a larger value of this slider favors prey and a smaller value favors killer whales.
•	The global constant hiding-effectiveness determines how effective prey are at hiding.  Specifically, it represents the proportion of prey that will not be seen by a whale when prey are hiding in the given patch.  When prey are hiding, the encounter rate is multiplied by (1- hiding-effectiveness) to determine the number of prey encountered thus a larger value of this slider favors prey and a smaller value favors killer whales.
•	The global constant hiding-time determines how long in hours that prey hiding after whales have hunted within the hunting-radius of that patch, thus a larger value of this slider favors prey and a smaller value favors killer whales.

About Encounter Rates

The other most important value that determines the effectiveness of hunting whales—and thus has the greatest impact on population dynamics of both predator and prey—is the prey-specific encounter rate, which is entered from prey-specific data files. This rate is a multiplicative factor that determines the number of prey a hunting pod of whales will encounter as a function of the density of the prey – that is, the number of prey in a single spatial unit: a .36 km2 patch.  The encounter rate R' entered from the file data is the rate used by the non-spatial model of Testa, Mock, et al. That encounter rate was fine-tuned as described in the paper "Agent-based modeling of the dynamics of mammal-eating killer whales and their prey" [TKTKCW].  In that model, the number E' of a given prey type a whale group sees in a day is computed as E'=P'*R' where P' is the global count of the given type of prey and R' is the encounter rate specific to that type of prey.

Our spatially-explicit model requires a new encounter rate R based on a much finer spatial and temporal granularity.  Rather than starting from scratch to experimentally determine a new rate, we applied a multiplicative transformation factor T to the existing encounter rate to compute R=T*R'. The value of T was based on several (reasoned) assumptions and some guesswork, and the result will certainly have to be adjusted and fine-tuned. 

Before explaining the transformation, it is worth considering units. Let P = prey count, R = encounter rate, and let E be the number of prey actually encountered. As with the previous model, E is the value we want to compute. E=P*R. Note the following.
•	P, though given as a prey count, is actually a density of prey—that is, a number of prey over a certain area. Its units are thus (prey / km2). 
•	E should give us a number of prey that are encountered by the hunting group in a given amount of time.  So units should be (prey / hr).   Note. It could be considered as (prey / hr * group), but the group is assumed.
•	It follows that the units for the ecnounter rate R must (km2 / hr).   Intuitively this makes sense, as it relates to the amount of space a whale group can search in an hour.  (As with the above, it could be considered as km2 / hr *group but the group is assumed.)

The previous model of [TKTKCW], though it does not explicitly represent as a spatial model, can be considered a very coarse spatial granularity where all of SE Alaska is a single spatial unit. The temporality granularity of the earlier project was units of one day.  Again, let E'=P'*R' be the values used in the previous model.  So at the simplest level, the transformation R=T*R' must adjust the encounter rate R' used in the previous "non-spatial" model (which is a rate for all of SE Alaska for 24 hours of hunting) for a different unit of time.  The first step seems simple: to change the rate from (prey / day) to (prey / hr) we multiply by (1 day / 24 hours). That is we start by setting T=1/24 and thus R=R'/24.

However the count in P' was also assumed to have a different area, and thus a different density. Our value of P is the count of prey is only ~.36km2. We must multiply by approximately 370,000 to scale that to a prey count for the region of SE Alaska being modeled. So a first approximately for T is T=370000/24.

  Unfortunately this simplification fails in both temporal and spatial regards by assuming non-realistic simplifications. First, whales do not hunt for all 24 hours of a day. Rather than dividing by 24, we should divide by the average number of hours a day a whale actually hunts. Second, prey are not distributed uniformly, and whales do not hunt in random locations but (presumably) in areas of dense distribution.  To account for that, we approximate the number of hours per day a whale hunts using the max-time-without-rest and rest-time sliders described above. Second, we compute a typical dense prey patch rather than a uniform distribution of prey, and use the ratio of that prey patch to the global populate count as the multiplicative factor. This is all still a rough estimate to be fine-tuned.

## HOW IT WORKS: WHALE MEMORY AND IMPORTANT RELATED CONSTANTS AND SLIDERS

Each whale keeps an updated a memory of past hunting experiences.  The world is divided into approximately 70 hunting regions. (To color the world by hunting region, set the map-mode to "hunting regions" and press <redraw>.)  A whale's year is divided into a number of hunting seasons whose length is determined by the slider days-per-hunting-season (plus one final hunting season at the end of the year for any leftover days, if days-per-hunting-season does not divide 365.) Thus if days-per-hunting-season=100, a whale will have season 0 running from day 0 to day 99, season 1 running from day 100 to day 199, season 2 running from day 200 to day 299, and season 3 running from day 300 to day 364. 

For each hunting season, a whale keeps track of every hunting region in which it has hunted during that season, and for each such season has a total number of kgs of food consumed and a total number of hours hunted. Thus the memory tells a whale for each season the average  (kgs/hour) of prey hunted and consumed for each hunting region it has hunted during that season. 

When whale's decide where to go next, they evaluate the current hunting conditions where they are in comparison with all the remembered hunting regions for that season. The slide evaluation-period specifies the number of days a whale uses to determine the best place to hung. For each remembered hunting region during that season, it will look at the total time that it could spend hunting at each remembered location (taking into account travel time) and multiply that by the average  kgs/hour expected by hunting there. The metabolic cost of kgs lost is subtracted. And the optimal location is chosen. 

Thus a smaller value for evaluation-period is less likely to value traveling a long distance in order to find good hunting.

## HOW IT WORKS: LONG DISTANCE TRAVEL

Long distance travel is computer based on a graph of approximately 1000 nodes spread over the world, and approximately 70 special nodes called anchor nodes. Each patch has information about which graph node is closest, and each graph node has information to find the shortest path along the graph to each anchor node. A path from a patch in one hunting region to a patch in another will go onto the travel graph, through an anchor node, and then reverse that process to the other patch.  The travel graph can be visualized using the map-node.  

(Note:  Since all long-distance travel will move along the graph and through an anchor node, the paths followed by whales can be adjusted to give a stochastically better fit to known paths discovered from whale-tracking data.)

## THINGS TO NOTICE

Mortality rates, and prey population dynamics.

## THINGS TO TRY

Experiment with the values of sliders and global constants. It will be important to fine-tune the factors that impact population dynamics by impacting predation success. It is not recommended that you adjust the sliders during a run. Set the sliders, watch what happens, then adjust.  

## EXTENDING THE MODEL

(* Future enhancements will include whale group social dynamics such as fusion and fission events. *)

## NETLOGO FEATURES


## RELATED MODELS



## CREDITS AND REFERENCES

Matthew Dickerson and Thomas Dickerson
Some of the long-lat coordinate transformation code written by Karly Wenz
Non-spatial model adaptation based on code from Kenrick Mock and the paper "Agent-based Modeling of the dynamics of mammal-eating killer whales and their prey" by Testa, Mock, Taylor, Koyuk, Coyle, and Waggoner.
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
