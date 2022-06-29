(ns firestone.core
  (:require [ysera.test :refer [is is-not is= error?]]
            [ysera.error :refer [error]]
            [ysera.collections :refer [seq-contains?]]
            [ysera.random :refer [get-random-int]]
            [firestone.definitions :refer [get-definition]]
            [firestone.construct :refer :all]))

(defn do-on-minion-damages
  [state player-id]
  (maybe-do-minion-abilities state player-id :on-minion-damage))

(defn do-on-hero-damages
  [state]
  (-> (maybe-do-minion-abilities state "p1" :on-hero-damage)
      (maybe-do-minion-abilities "p2" :on-hero-damage)))

(defn do-on-minion-heals
  [state]
  (-> (maybe-do-minion-abilities state "p1" :on-minion-heal)
      (maybe-do-minion-abilities "p2" :on-minion-heal)))

(defn do-on-character-heals
  [state]
  (-> (maybe-do-minion-abilities state "p1" :on-character-heal)
      (maybe-do-minion-abilities "p2" :on-character-heal)))


(defn get-health
  "Returns the health of the character."
  {:test (fn []
           ; Uninjured minion
           (is= (-> (create-minion "Flame Imp")
                    (get-health))
                2)
           ; Injured minion
           (is= (-> (create-minion "Flame Imp" :damage-taken 1)
                    (get-health))
                1)
           ; Minion in a state with health gain
           (is= (-> (create-game [{:minions [(create-minion "Boulderfist Ogre" :id "bo")]}])
                    (update-minion "bo" :health-gain 1)
                    (get-health "bo"))
                8)
           ; Uninjured hero
           (is= (-> (create-hero "Anduin Wrynn")
                    (get-health))
                30)
           ; Injured hero
           (is= (-> (create-hero "Anduin Wrynn" :damage-taken 2)
                    (get-health))
                28)
           ; Hero in a state
           (is= (-> (create-game [{:hero (create-hero "Anduin Wrynn" :id "h1")}])
                    (get-health "h1"))
                30))}
  ([character]
   {:pre [(map? character) (contains? character :damage-taken)]}
   {:pre [(map? character) (contains? character :health-gain)]}
   (let [definition (get-definition character)]
     (- (+ (:health definition) (:health-gain character))
        (:damage-taken character))))
  ([state id]
   (get-health (get-character state id))))

(defn get-attack
  "Returns the attack of the character with the given id."
  {:test (fn []
           (is= (-> (create-game [{:minions [(create-minion "Boulderfist Ogre" :id "bo")]}])
                    (get-attack "bo"))
                6)
           (is= (-> (create-game [{:minions [(create-minion "Boulderfist Ogre" :id "bo")]}])
                    (update-minion "bo" :attack-gain 1)
                    (get-attack "bo"))
                7)
           (is= (-> (create-game [(create-hero "Uther Lightbringer" :id "h1")
                                  {:hero (create-hero "Anduin Wrynn" :id "h2")
                                   :minions [(create-minion "Boulderfist Ogre" :id "bo")
                                             (create-minion "Unstable Ghoul" :id "ug")]}])
                    (get-attack "h1"))
                0))}
  [state id]
  (let [character (get-character state id)
        definition (get-definition (:name character))]
    (+ (or (:attack definition) 0) (or (:attack-gain character) 0))))

(defn add-attack-gain
  [state minion-id attack-amount]
  (update-minion state minion-id :attack-gain (fn [x] (+ x attack-amount))))

(defn add-health-gain
  [state minion-id attack-amount]
  (update-minion state minion-id :health-gain (fn [x] (+ x attack-amount))))

(defn minion-has-attribute?
  {:test (fn []
           (-> (create-game [{:minions [(create-minion "Archmage Antonidas" :id "aa")]}])
               (minion-has-attribute? "aa" :on-spell-cast)
               (is))
           (-> (create-game [{:minions [(create-minion "Archmage Antonidas" :id "aa")]}])
               (minion-has-attribute? "aa" :bad-attribute)
               (is-not)))}
  [state id-or-entity attribute]
  {:pre [(or (string? id-or-entity)
             (and (map? id-or-entity)
                  (contains? id-or-entity :name)))]}
  (let [minion-id (if (string? id-or-entity)
                    id-or-entity
                    (:id id-or-entity))
        minion (get-character state minion-id)
        minion-def (get-definition minion)]
    (or (some? (attribute minion))
        (some? (attribute minion-def)))))

(defn sleepy?
  "Checks if the minion with given id is sleepy."
  {:test (fn []
           (is (-> (create-game [{:minions [(create-minion "Boulderfist Ogre" :id "bo")]}]
                                :minion-ids-summoned-this-turn ["bo"])
                   (sleepy? "bo")))
           (is-not (-> (create-game [{:minions [(create-minion "Boulderfist Ogre" :id "bo")]}])
                       (sleepy? "bo")))
           ; Since Militia Commander has Rush, it should NOT get sleepy!
           (is-not (-> (create-game [{:minions [(create-minion "Militia Commander" :id "mc")]}]
                                    :minion-ids-summoned-this-turn ["mc"])
                       (sleepy? "mc")))
           ; Since Dire Wolf Alpha has Rush, it should NOT get sleepy!
           (is-not (-> (create-game [{:minions [(create-minion "Dire Wolf Alpha" :id "dw")]}]
                                    :minion-ids-summoned-this-turn ["dw"])
                       (sleepy? "dw"))))}
  [state id]
  (and (not (minion-has-attribute? state id :rush))
       (seq-contains? (:minion-ids-summoned-this-turn state) id)))

(defn can-attack?
  [state attacker-id]
  {:test (fn []
           ; Should be able to attack if a minion can attack
           (is (-> (create-game [{:minions [(create-minion "Boulderfist Ogre" :id "bo")]}])
                   (can-attack? "bo")))
           ; Should be able to attack if a minion cannot attack
           (is-not (-> (create-game)
                       (add-minion-to-board "p1" (create-minion "Ragnaros the Firelord" :id "ug") 0)
                       (can-attack? "ug")))
           ; Should not be able to attack if a player is not in his turn
           (is-not (-> (create-game [(create-hero "Uther Lightbringer" :id "h1")
                                     {:hero (create-hero "Anduin Wrynn" :id "h2")
                                      :minions [(create-minion "Boulderfist Ogre" :id "bo")
                                                (create-minion "Unstable Ghoul" :id "ug")]}])
                       (can-attack? "bo")))
           ; A hero should not be able to attack if a hero attack is 0
           (is-not (-> (create-game [(create-hero "Uther Lightbringer" :id "h1")
                                     {:hero (create-hero "Anduin Wrynn" :id "h2")
                                      :minions [(create-minion "Boulderfist Ogre" :id "bo")
                                                (create-minion "Unstable Ghoul" :id "ug")]}])
                       (can-attack? "h1"))))}
  (let [attacker (get-character state attacker-id)]
    (and attacker
         (not (sleepy? state attacker-id))
         (= (:attacks-performed-this-turn attacker) 0)
         (= (:player-id-in-turn state) (get-owner state attacker-id))
         (not (= (get-attack state attacker-id) 0))
         (not (true? (:cannot-attack attacker))))))

(defn valid-attack?
  "Checks if the attack is valid"
  {:test (fn []
           ; Should be able to attack an enemy minion
           (is (-> (create-game [{:minions [(create-minion "Boulderfist Ogre" :id "bo")]}
                                 {:minions [(create-minion "Injured Blademaster" :id "ib")]}])
                   (valid-attack? "p1" "bo" "ib")))
           ; Should be able to attack an enemy hero
           (is (-> (create-game [{:minions [(create-minion "Boulderfist Ogre" :id "bo")]}])
                   (valid-attack? "p1" "bo" "h2")))
           ; Should not be able to attack your own minions
           (is-not (-> (create-game [{:minions [(create-minion "Boulderfist Ogre" :id "bo")
                                                (create-minion "Injured Blademaster" :id "ib")]}])
                       (valid-attack? "p1" "bo" "ib")))
           ; Should not be able to attack if it is not your turn
           (is-not (-> (create-game [{:minions [(create-minion "Boulderfist Ogre" :id "bo")]}
                                     {:minions [(create-minion "Injured Blademaster" :id "ib")]}]
                                    :player-id-in-turn "p2")
                       (valid-attack? "p1" "bo" "ib")))
           ; Should not be able to attack if you are sleepy
           (is-not (-> (create-game [{:minions [(create-minion "Boulderfist Ogre" :id "bo")]}
                                     {:minions [(create-minion "Injured Blademaster" :id "ib")]}]
                                    :minion-ids-summoned-this-turn ["bo"])
                       (valid-attack? "p1" "bo" "ib")))
           ; Should not be able to attack if you already attacked this turn
           (is-not (-> (create-game [{:minions [(create-minion "Boulderfist Ogre" :id "bo" :attacks-performed-this-turn 1)]}
                                     {:minions [(create-minion "Injured Blademaster" :id "ib")]}])
                       (valid-attack? "p1" "bo" "ib")))
           ; Should not be able to attack if a minion cannot attack
           (is-not (-> (create-game [{:minions [(create-minion "Ragnaros the Firelord" :id "bo")]}
                                     {:minions [(create-minion "Injured Blademaster" :id "ib")]}]
                                    :minion-ids-summoned-this-turn ["bo"])
                       (valid-attack? "p1" "bo" "ib"))))}
  [state player-id attacker-id target-id]
  (let [attacker (get-minion state attacker-id)
        target (get-character state target-id)
        player-change-fn {"p1" "p2"
                          "p2" "p1"}
        opponent-player-id (player-change-fn player-id)
        taunts (get-taunts state opponent-player-id)
        taunts-ids (map :id taunts)]
    (and attacker
         target
         (can-attack? state attacker-id)
         (= (:player-id-in-turn state) player-id)
         (< (:attacks-performed-this-turn attacker) 1)
         (not (sleepy? state attacker-id))
         (not= (:owner-id attacker) (:owner-id target))
         (not (and
                (not= (count taunts) 0)
                (not (.contains taunts-ids target-id)))))))

(defn fatigue?
  "Check if a player is fatigue"
  {:test (fn []
           ; A player should not be fatigue if his deck is not empty.
           (is-not (-> (create-game [{:minions ["Boulderfist Ogre"]
                                  :deck    ["Injured Blademaster"]
                                  :hand    ["Silver Hand Recruit"]}
                                 {:hero "Anduin Wrynn"}]
                                :player-id-in-turn "p1")
                   (fatigue? "p1")))
           ; A player should be fatigue if his deck is empty.
           (is (-> (create-game [{:minions ["Boulderfist Ogre"]
                                      :deck    ["Injured Blademaster"]
                                      :hand    ["Silver Hand Recruit"]}
                                     {:hero "Anduin Wrynn"}]
                                    :player-id-in-turn "p1")
                       (fatigue? "p2"))))}
  [state player-id]
  (= (get-num-from-deck state player-id) 0))

(defn get-hero-id
  "Return hero id by a player-id"
  {:test (fn []
           ; Should given right hero id
           (is= (-> (create-game [{:hero (create-hero "Anduin Wrynn" :id "h1")}])
                    (get-hero-id "p1"))
                "h1"))}
  [state player-id]
  (get-in state [:players player-id :hero :id]))

(defn hero-power-available?
  {:test (fn []
           (is= (-> (create-empty-state)
               (hero-power-available? "p1")) true))}
  [state player-id]
  (-> (get-player state player-id)
      (:hero)
      (:hero-power-available)))

(defn add-to-character-attribute
  "docstring"
  [state character-id attribute amount-to-add max]
  )

(defn attack-hero
  "Return the state after a hero is attacked by attack points."
  {:test (fn []
           ; Should record attack on target minion
           (is= (-> (create-game [{:hero (create-hero "Anduin Wrynn" :id "h1")}])
                    (attack-hero "p1" 2)
                    (attack-hero "h1" 2)
                    (get-health "h1"))
                26)
           ; Should game over after player's hero dead
           (is= (-> (create-game [{:hero (create-hero "Anduin Wrynn" :id "h1")}])
                    (attack-hero "p1" 999))
                nil))}
  [state player-id-or-hero-id attack]
  (as-> (update-hero state player-id-or-hero-id :damage-taken (fn [x] (+ x attack))) $
        (do-on-hero-damages $)
        (if (<= (get-health (get-hero $ player-id-or-hero-id)) 0)
          (println "Game Over!")
          $)))

(defn heal-hero
  {:test (fn []
           ; Should record attack on target minion
           (is= (-> (create-game [{:hero (create-hero "Anduin Wrynn" :id "h1")}])
                    (attack-hero "p1" 2)
                    (heal-hero "p1" 4)
                    (get-health "h1"))
                30)
           )}
  [state player-id-or-hero-id heal-amount]
  (update-hero state player-id-or-hero-id :damage-taken (fn [x] (max 0 (- x heal-amount)))))

(defn attack-minion
  "Return the state after a minion is attacked by attack points."
  {:test (fn []
           ; Should record attack on target minion
           (is= (-> (create-game [{:minions [(create-minion "Boulderfist Ogre" :id "bo")]}])
                    (attack-minion "bo" 2)
                    (get-health "bo"))
                5)
           ; Should record attack on target minion and remove them from the state
           (is= (-> (create-game [{:minions [(create-minion "Boulderfist Ogre" :id "bo")]}])
                    (attack-minion "bo" 8)
                    (get-character "bo"))
                nil))}
  [state minion-id attack]
  (let [minion (get-minion state minion-id)
        owner-id (:owner-id minion)
        maybe-remove-minion (fn [state minion-id]  (if (<= (get-health state minion-id) 0)
                                                    (remove-minion state minion-id)
                                                    state))]
    (-> (update-minion state minion-id :damage-taken (fn [x] (+ x attack)))
        (do-on-minion-damages owner-id)
        (maybe-do-minion-ability minion-id :on-damage)
        (maybe-remove-minion minion-id))))

(defn heal-minion
  {:test (fn []
           ; Should record attack on target minion
           (is= (-> (create-game [{:minions [(create-minion "Boulderfist Ogre" :id "bo")]}])
                    (attack-minion "bo" 2)
                    (heal-minion "bo" 4)
                    (get-health "bo"))
                7)
           )}
  [state minion-id heal-amount]
  {:pre [(pos? heal-amount)]}
  (let [heal-possible? (not= 0 (get-damage-taken state minion-id))]
    (if heal-possible?
      (-> (update-minion state minion-id :damage-taken (fn [x] (max 0 (- x heal-amount))))
          (do-on-minion-heals))
      state)))

(defn attack-character
  {:test (fn []
           ; Should record attack on target minion
           (is= (-> (create-game [{:hero (create-hero "Anduin Wrynn" :id "h1")}])
                    (attack-character "h1" 2)
                    (get-health "h1"))
                28)
           )}
  [state character-id attack]
  (let [character (get-character state character-id)]
    (if (nil? character)
        (error "Character not found!")
        (condp = (:entity-type character)
          :hero (attack-hero state character-id attack)
          :minion (attack-minion state character-id attack)))))

(defn heal-character
  {:test (fn []
           ; Should record attack on target minion
           (is= (-> (create-game [{:minions [(create-minion "Boulderfist Ogre" :id "bo")]}])
                    (attack-character "bo" 2)
                    (heal-character "bo" 4)
                    (get-health "bo"))
                7)
           )}
  [state character-id heal-amount]
  {:pre [(pos? heal-amount)]}
  (let [character (get-character state character-id)
        heal-possible? (not= 0 (get-damage-taken state character-id))]
    (if (and character heal-possible?)
      (-> (condp = (:entity-type character)
            :hero (heal-hero state character-id heal-amount)
            :minion (heal-minion state character-id heal-amount))
          (do-on-character-heals))
      state)))

(defn attack-characters
  {:test (fn []
           (is= (as-> (create-game [{:hero    (create-hero "Anduin Wrynn" :id "aw")
                                   :minions [(create-minion "Boulderfist Ogre" :id "bo")]}]) $
                    (attack-characters $ 1 "aw" "bo")
                    (get-characters $ "p1")
                    (map :damage-taken $))
              (list 1 1)))}
  [state attack & ids]
  (reduce (fn [state id] (attack-character state id attack)) state ids))

(defn minion-attack-minion
  "Return the state after a minion attacks a minion."
  {:test (fn []
           ; Should record attack on the target minion after the minion attacks
           (is= (-> (create-game [{:minions [(create-minion "Boulderfist Ogre" :id "bo")]}
                                 {:minions [(create-minion "Injured Blademaster" :id "ib")]}])
                    (minion-attack-minion "p1"  "bo" "ib")
                    (get-health "ib"))
                1)
           ; Should record attack on the attacker minion after the minion attacks
           (is= (-> (create-game [{:minions [(create-minion "Boulderfist Ogre" :id "bo")]}
                                  {:minions [(create-minion "Injured Blademaster" :id "ib")]}])
                    (minion-attack-minion "p1" "bo" "ib")
                    (get-health "bo"))
                3)
           ; Should record death on the target minion after the minion attacks
           (is= (-> (create-game [{:minions [(create-minion "Boulderfist Ogre" :id "bo")]}
                                  {:minions [(create-minion "Flame Imp" :id "fi")]}])
                    (minion-attack-minion "p1" "bo" "fi")
                    (get-character "fi"))
                nil)
           ; Should record death on the attacker minion after the minion attacks
           (is= (-> (create-game [{:minions [(create-minion "Flame Imp" :id "fi")]}
                                  {:minions [(create-minion "Boulderfist Ogre" :id "bo")]}])
                    (minion-attack-minion "p1" "fi" "bo")
                    (get-character "fi"))
                nil))}
  [state attacker-player-id attacker-minion-id target-minion-id]
  (if (valid-attack? state attacker-player-id attacker-minion-id target-minion-id)
    (let [attacker-attack (get-attack state attacker-minion-id)
          target-attack (get-attack state target-minion-id)
          player-change-fn {"p1" "p2"
                            "p2" "p1"}
          target-player-id (player-change-fn attacker-player-id)
          attacker-has-lifesteal? (minion-has-attribute? state attacker-minion-id :lifesteal)
          target-has-lifesteal? (minion-has-attribute? state target-minion-id :lifesteal)]
      (as-> (update-minion state attacker-minion-id :attacks-performed-this-turn inc)$
            (attack-minion $ target-minion-id attacker-attack)
            (attack-minion $ attacker-minion-id target-attack)
            (cond-> $
                    attacker-has-lifesteal? (heal-hero attacker-player-id attacker-attack)
                    target-has-lifesteal? (heal-hero target-player-id target-attack))))
    state))

(defn minion-attack-hero
  "Return the state after a minion attacks a hero. Use this after checking if it is a valid attack"
  {:test (fn []
           ; Should record attack on the minion after the minion attacks
           (is= (-> (create-game [{:minions [(create-minion "Boulderfist Ogre" :id "bo")]}
                                  {:minions [(create-minion "Injured Blademaster" :id "ib")]}]
                                 :player-id-in-turn "p1")
                    (minion-attack-hero "bo" "p2")
                    (get-minion "bo")
                    (:attacks-performed-this-turn))
                1)
           ; Should record damage-taken on the hero after the minion attacks
           (is= (-> (create-game [{:minions [(create-minion "Boulderfist Ogre" :id "bo")]}
                                  {:minions [(create-minion "Injured Blademaster" :id "ib")]}]
                                 :player-id-in-turn "p1")
                    (minion-attack-hero "bo" "p2")
                    (get-in [:players "p2" :hero :damage-taken]))
                6))}
  [state minion-id target-player-id]
  (let [attack (get-attack state minion-id)]
    (as-> (update-minion state minion-id :attacks-performed-this-turn inc) $
          (update-in $ [:players target-player-id :hero :damage-taken] + attack))))

(defn pay-mana
  {:test (fn []
           (is= (-> (create-game)
                    (pay-mana "p1" (create-card "Squirrel"))
                    (get-mana "p1"))
                9))}
  [state player-id card]
  (let [definition (get-definition card)
        mana-cost (or (:mana-cost definition) 0)]
    (update-mana state player-id (fn [old-value] (- old-value mana-cost)))))

(defn get-valid-attack-ids
  {:test (fn []
           ; Should attack only one minion with taunt
           (-> (create-game [{:minions ["Boulderfist Ogre" "Archmage Antonidas" "Injured Blademaster"]
                              :deck    ["Injured Blademaster" "Mukla's Champion" "Nerubian Ambush!"]
                              :hand    ["Silver Hand Recruit" "Lowly Squire" "Sprint" "The Black Knight"]}
                             {:hero "Anduin Wrynn"
                              :minions ["Boulderfist Ogre" "Argent Watchman" "Archmage Antonidas" "Unstable Ghoul" "Injured Blademaster"]
                              :deck    ["Injured Blademaster" "Mukla's Champion" "Nerubian Ambush!"]
                              :hand    ["Silver Hand Recruit" "Lowly Squire" "Sprint" "The Black Knight"]}]
                            :player-id-in-turn "p1")
               (get-valid-attack-ids "p1")
               (count)
               (is= 1))
           ; Should attack all minions and hero of opponent
           (-> (create-game [{:minions ["Boulderfist Ogre" "Archmage Antonidas" "Injured Blademaster"]
                              :deck    ["Injured Blademaster" "Mukla's Champion" "Nerubian Ambush!"]
                              :hand    ["Silver Hand Recruit" "Lowly Squire" "Sprint" "The Black Knight"]}
                             {:hero "Anduin Wrynn"
                              :minions ["Boulderfist Ogre" "Argent Watchman" "Archmage Antonidas" "Injured Blademaster"]
                              :deck    ["Injured Blademaster" "Mukla's Champion" "Nerubian Ambush!"]
                              :hand    ["Silver Hand Recruit" "Lowly Squire" "Sprint" "The Black Knight"]}]
                            :player-id-in-turn "p1")
               (get-valid-attack-ids "p1")
               (count)
               (is= 5)))}
  [state player-id]
  (let [player-change-fn {"p1" "p2"
                          "p2" "p1"}
        opponent-player-id (player-change-fn player-id)
        taunts (get-taunts state opponent-player-id)
        opponent-hero-id (get-hero-id state opponent-player-id)]
    (if (= (count taunts) 0)
      (-> (map (fn [p]
                 (:id p))
               (get-minions state opponent-player-id))
          (vec)
          (conj opponent-hero-id))
      (-> (map (fn [p]
                 (:id p))
               taunts)
          (vec)))))

(defn get-valid-target-ids
  "Gets the valid target ids for either a spell card or a character ability."
  [state player-id name-or-entity]
  (let [definition (get-definition name-or-entity)
        name (:name definition)
        equal-or-contains (fn [case name] (or (= name case)
                                              (and (coll? case)
                                                   (seq-contains? case name))))
        player-change-fn {"p1" "p2"
                          "p2" "p1"}
        opponent-player-id (player-change-fn player-id)]
    (->> (condp equal-or-contains name
        ; Target: Any minion with deathrattle
        "Princess Huhuran"         (->> (get-minions state player-id)
                                        (filter (fn [m] (:deathrattle (get-definition m)))))
        ; Target: Any minion
        ["Ancestral Spirit"
         "Faceless Manipulator"]   (get-minions state)
        ; Target: Any character
        ["Rocket Boots"
         "Fireball"]               (get-characters state)
        ; Target: Damaged characters
        ["Lesser Heal"
         "Light of the Naaru"]     (->> (get-characters state)
                                        (filter (fn [c] (not= 0 (:damage-taken c)))))
        ; Target: Undamaged minions
        "Backstab"                 (->> (get-minions state)
                                        (filter (fn [c] (= 0 (:damage-taken c)))))
        ; Target: Enemy minions
        ["Convert"
         "Entomb"]                 (get-minions state opponent-player-id)
        ;else
        [])
        (map :id)
        (vec))))

(defn get-states
  ;"aura"
  ;"deathrattle"      -
  ;"divine-shield"
  ;"effect"           -
  ;"elusive"
  ;"enrage"
  ;"frozen"
  ;"immune"
  ;"inspire"          -
  ;"lifesteal"        -
  ;"mega-windfury"
  ;"poisonous"
  ;"silenced"
  ;"spell-damage"
  ;"stealth"
  ;"taunt"            -
  ;"windfury"
  {:test (fn []
           (let [states (let [state (create-game [{:minions ["Unstable Ghoul"]}])]
                          (->> (get-minions state)
                               (first)
                               (get-states state)))]
             (is= states ["taunt" "deathrattle"]))
           (let [states (let [state (create-game [{:minions ["Argent Watchman"]}])]
                          (->> (get-minions state)
                               (first)
                               (get-states state)))]
             (is= states ["inspire"])))}
  [state character]
  (let [character-name (:name character)
        definition (get-definition character-name)
        states []]
    (cond-> states
            (:taunt definition) (conj "taunt")
            (:deathrattle definition) (conj "deathrattle")
            (:inspire definition) (conj "inspire")
            (:lifesteal definition) (conj "lifesteal")
            (or (:on-damage definition)
                (:on-spell-cast definition)
                (:on-hero-damage definition)
                (:on-minion-damage definition)
                (:on-summon definition)
                (:on-character-heal definition)
                (:on-minion-heal definition)) (conj "effect"))))

(defn get-valid-spell-ids
  {:test (fn []
           ; Should attack only one minion with taunt
           (-> (create-game [{:minions ["Boulderfist Ogre" "Archmage Antonidas" "Injured Blademaster"]
                              :deck    ["Injured Blademaster" "Mukla's Champion" "Nerubian Ambush!"]
                              :hand    ["Silver Hand Recruit" "Lowly Squire" "Sprint" "The Black Knight"]}
                             {:hero "Anduin Wrynn"
                              :minions ["Boulderfist Ogre" "Argent Watchman" "Archmage Antonidas" "Unstable Ghoul" "Injured Blademaster"]
                              :deck    ["Injured Blademaster" "Mukla's Champion" "Nerubian Ambush!"]
                              :hand    ["Silver Hand Recruit" "Lowly Squire" "Sprint" "The Black Knight"]}]
                            :player-id-in-turn "p1")
               (get-valid-spell-ids)
               (count)
               (is= 10)))}
  [state]
  (-> (map (fn [p]
             (:id p))
           (get-characters state "p1"))
      (concat (map (fn [p]
                     (:id p))
                   (get-characters state "p2")))
      (vec)))
