(ns firestone.construct
  (:require [ysera.test :refer [is is-not is= error?]]
            [ysera.random :refer [get-random-int]]
            [firestone.definitions :refer [get-definition]]))

(defn create-hero
  "Creates a hero from its definition by the given hero name. The additional key-values will override the default values."
  {:test (fn []
           (is= (create-hero "Uther Lightbringer" :id "h1")
                {:name         "Uther Lightbringer"
                 :entity-type  :hero
                 :damage-taken 0
                 :id "h1"
                 :hero-power-available true
                 :hero-power "Reinforce"
                 :attacks-performed-this-turn 0
                 :health-gain  0})

           (is= (create-hero "Uther Lightbringer" :damage-taken 10)
                {:name         "Uther Lightbringer"
                 :entity-type  :hero
                 :damage-taken 10
                 :hero-power-available true
                 :hero-power "Reinforce"
                 :attacks-performed-this-turn 0
                 :health-gain  0}))}
  [name & kvs]
  (let [hero {:name         name
              :entity-type  :hero
              :damage-taken 0
              :hero-power-available true
              :hero-power (:hero-power (get-definition name))
              :health-gain  0
              :attacks-performed-this-turn 0}]
    (if (empty? kvs)
      hero
      (apply assoc hero kvs))))

(defn create-card
  "Creates a card from its definition by the given card name. The additional key-values will override the default values."
  {:test (fn []
           (is= (create-card "Boulderfist Ogre" :id "bo")
                {:id          "bo"
                 :entity-type :card
                 :name        "Boulderfist Ogre"}))}
  [name & kvs]
  (let [card {:name        name
              :entity-type :card}]
    (if (empty? kvs)
      card
      (apply assoc card kvs))))

(defn create-minion
  "Creates a minion from its definition by the given minion name. The additional key-values will override the default values."
  {:test (fn []
           (is= (create-minion "Boulderfist Ogre" :id "bo" :attacks-performed-this-turn 1)
                {:attacks-performed-this-turn 1
                 :damage-taken                0
                 :health-gain                 0
                 :entity-type                 :minion
                 :name                        "Boulderfist Ogre"
                 :id                          "bo"}))}
  [name & kvs]
  (let [definition (get-definition name)                    ; Will be used later
        minion {:damage-taken                0
                :health-gain                 0
                :entity-type                 :minion
                :name                        name
                :attacks-performed-this-turn 0}]
    (if (empty? kvs)
      minion
      (apply assoc minion kvs))))

(defn create-empty-state
  "Creates an empty state with the given heroes."
  {:test (fn []
           ; Rexxar will be the default hero
           (is= (create-empty-state [(create-hero "Uther Lightbringer")
                                     (create-hero "Uther Lightbringer")])
                (create-empty-state))

           (is= (create-empty-state [(create-hero "Uther Lightbringer" :id "r")
                                     (create-hero "Anduin Wrynn")])
                {:player-id-in-turn             "p1"
                 :players                       {"p1" {:id      "p1"
                                                       :deck    []
                                                       :hand    []
                                                       :minions []
                                                       :mana    10
                                                       :fatigue-counter 0
                                                       :graveyard []
                                                       :taunts  []
                                                       :hero    {:name         "Uther Lightbringer"
                                                                 :id           "r"
                                                                 :damage-taken 0
                                                                 :hero-power-available true
                                                                 :hero-power "Reinforce"
                                                                 :health-gain  0
                                                                 :attacks-performed-this-turn 0
                                                                 :entity-type  :hero}}
                                                 "p2" {:id      "p2"
                                                       :deck    []
                                                       :hand    []
                                                       :minions []
                                                       :mana    10
                                                       :fatigue-counter 0
                                                       :graveyard []
                                                       :taunts  []
                                                       :hero    {:name         "Anduin Wrynn"
                                                                 :id           "h2"
                                                                 :damage-taken 0
                                                                 :hero-power-available true
                                                                 :hero-power "Lesser Heal"
                                                                 :health-gain  0
                                                                 :attacks-performed-this-turn 0
                                                                 :entity-type  :hero}}}
                 :counter                       1
                 :minion-ids-summoned-this-turn []
                 :action-index                  0
                 :event                         {:name           "start-of-game"}
                 :seed 888}))}
  ([heroes]
   ; Creates Uther Lightbringer heroes if heroes are missing.
   (let [heroes (->> (concat heroes [(create-hero "Uther Lightbringer")
                                     (create-hero "Uther Lightbringer")])
                     (take 2))]
     {:player-id-in-turn             "p1"
      :players                       (->> heroes
                                          (map-indexed (fn [index hero]
                                                         {:id      (str "p" (inc index))
                                                          :deck    []
                                                          :hand    []
                                                          :minions []
                                                          :mana    10
                                                          :fatigue-counter 0
                                                          :graveyard []
                                                          :taunts  []
                                                          :hero    (if (contains? hero :id)
                                                                     hero
                                                                     (assoc hero :id (str "h" (inc index))))}))
                                          (reduce (fn [a v]
                                                    (assoc a (:id v) v))
                                                  {}))
      :counter                       1
      :minion-ids-summoned-this-turn []
      :action-index                  0
      :event                         {:name           "start-of-game"}
      :seed 888}))
  ([]
   (create-empty-state [])))

(declare create-game)

(defn get-player
  "Returns the player with the given id."
  {:test (fn []
           (is= (-> (create-empty-state)
                    (get-player "p1")
                    (:id))
                "p1"))}
  [state player-id]
  (get-in state [:players player-id]))

(defn get-players
  {:test (fn []
           (is= (->> (create-game)
                     (get-players)



                     (map :id))
                ["p1" "p2"]))}
  [state]
  (->> (:players state)
       (vals)))

(defn get-heroes
  {:test (fn []
           (is= (->> (create-game [{:hero "Anduin Wrynn"}])
                     (get-heroes)
                     (map :name))
                ["Anduin Wrynn" "Uther Lightbringer"]))}
  [state]
  (->> (get-players state)
       (map :hero)))

(defn get-hero
  {:test (fn []
           (is= (-> (create-game [{:hero "Anduin Wrynn"}])
                    (get-hero "p1")
                    (:name))
                "Anduin Wrynn"))}
  [state player-id-or-hero-id]
  (-> (filter (fn [p]
                (or (= (:id p) player-id-or-hero-id)
                    (= (:id (:hero p)) player-id-or-hero-id)))
              (get-players state))
      (first)
      (:hero)))

(defn replace-hero
  {:test (fn []
           (is= (-> (create-game [{:hero (create-hero "Anduin Wrynn" :id "h")}])
                    (replace-hero (create-hero "Uther Lightbringer" :id "h"))
                    (get-player "p1")
                    (:hero)
                    (:name))
                "Uther Lightbringer"))}
  [state new-hero]
  (let [hero-id (:id new-hero)
        owner-id (->> (get-players state)
                      (filter (fn [p] (= (:id (:hero p)) hero-id)))
                      (first)
                      (:id))]
    (assoc-in state [:players owner-id :hero] new-hero)))

(defn update-hero
  {:test (fn []
           (is= (-> (create-game [{:hero (create-hero "Anduin Wrynn" :id "h")}])
                    (update-hero "h" :damage-taken 2)
                    (get-player "p1")
                    (:hero)
                    (:damage-taken))
                2))}
  [state player-id-or-hero-id key function-or-value]
  (let [hero (get-hero state player-id-or-hero-id)]
    (replace-hero state (if (fn? function-or-value)
                          (update hero key function-or-value)
                          (assoc hero key function-or-value)))))

(defn get-fatigue-counter
  "Return the counter for calculating damage-taken from fatigue"
  {:test (fn []
           ;Should be 0 in the beginning
           (is= (-> (create-empty-state)
                    (get-fatigue-counter "p1"))
                0))}
  [state player-id]
  (get-in state [:players player-id :fatigue-counter]))

(defn get-player-id-in-turn
  {:test (fn []
           (is= (-> (create-empty-state)
                    (get-player-id-in-turn))
                "p1"))}
  [state]
  (:player-id-in-turn state))

(defn get-minions
  "Returns the minions on the board for the given player-id or for both players."
  {:test (fn []
           ; Getting minions is also tested in add-minion-to-board.
           (is= (-> (create-empty-state)
                    (get-minions "p1"))
                [])
           (is= (-> (create-empty-state)
                    (get-minions))
                [])
           (is= (as-> (create-game [{:minions ["Boulderfist Ogre"]}]) $
                      (get-minions $ "p1")
                      (map :name $))
                ["Boulderfist Ogre"]))}
  ([state player-id]
   (:minions (get-player state player-id)))
  ([state]
   (->> (:players state)
        (vals)
        (map :minions)
        (apply concat))))

(defn get-minion
  "Returns the minion with the given id."
  {:test (fn []
           (is= (-> (create-game [{:minions [(create-minion "Boulderfist Ogre" :id "bo")]}])
                    (get-minion "bo")
                    (:name))
                "Boulderfist Ogre"))}
  [state id]
  (->> (get-minions state)
       (filter (fn [m] (= (:id m) id)))
       (first)))

(defn get-next-available-position
  {:test (fn []
           (is= (-> (create-game [{:minions [(create-minion "Boulderfist Ogre" :id "bo")
                                             (create-minion "Boulderfist Ogre" :id "bo")]}])
                    (get-next-available-position "p1"))
                2))}
  [state player-id]
  ; TODO: What if board is full?
  (->> (get-minions state player-id)
       (map :position)
       (apply max)
       (+ 1)))

(defn maybe-do-minion-ability
  "Executes an inspire, battlecry etc. but only if the minion exists and has the ability."
  {:test (fn []
           (-> (create-game [{:minions [(create-minion "Boneguard Lieutenant" :id "bl")]
                              :deck    ["Injured Blademaster"]
                              :hand    ["Sprint"]}] )
               (maybe-do-minion-ability "bl" :inspire)))}
  [state minion-id ability-key & {:keys [target-id]}]
  (if-let [minion (get-minion state minion-id)]
    (if-let [ability (ability-key (get-definition minion))]
      (let [minion-name (minion :name)
            player-id (minion :owner-id)
            player-change-fn {"p1" "p2"
                              "p2" "p1"}
            opponent-id (player-change-fn player-id)
            next-position (get-next-available-position state player-id)
            extra-inputs {:target-id   target-id
                          :minion-id   minion-id
                          :player-id   player-id
                          :minion-name minion-name
                          :opponent-id opponent-id
                          :next-position next-position}]
        (ability state extra-inputs))
      state)
    state))

(defn maybe-do-minion-abilities
  [state player-id ability-key & {:keys [target-id]}]
  (reduce (fn [state minion]
            (maybe-do-minion-ability state (:id minion) ability-key :target-id target-id))
          state
          (get-minions state player-id)))

(defn do-on-summons
  [state player-id]
  (maybe-do-minion-abilities state player-id :on-summon))

(defn replace-minion
  "Replaces a minion with the same id as the given new-minion."
  {:test (fn []
           (is= (-> (create-game [{:minions [(create-minion "Boulderfist Ogre" :id "m")]}])
                    (replace-minion (create-minion "Silver Hand Recruit" :id "m"))
                    (get-minion "m")
                    (:name))
                "Silver Hand Recruit")
           (is= (-> (create-game [{:minions [(create-minion "Boulderfist Ogre" :id "m")]}])
                    (replace-minion (create-minion "Silver Hand Recruit") "m")
                    (get-minion "m")
                    (:name))
                "Silver Hand Recruit"))}
  ([state new-minion]
   (let [owner-id (or (:owner-id new-minion)
                      (:owner-id (get-minion state (:id new-minion))))]
     (update-in state
                [:players owner-id :minions]
                (fn [minions]
                  (map (fn [m]
                         (if (= (:id m) (:id new-minion))
                           new-minion
                           m))
                       minions)))))
  ([state new-minion old-minion-id]
   (->> (assoc new-minion :id old-minion-id)
        (replace-minion state))))

(defn update-minion
  "Updates the value of the given key for the minion with the given id. If function-or-value is a value it will be the
   new value, else if it is a function it will be applied on the existing value to produce the new value."
  {:test (fn []
           (is= (-> (create-game [{:minions [(create-minion "Boulderfist Ogre" :id "bo")]}])
                    (update-minion "bo" :damage-taken inc)
                    (update-minion "bo" :damage-taken inc)
                    (get-minion "bo")
                    (:damage-taken))
                2)
           (is= (-> (create-game [{:minions [(create-minion "Boulderfist Ogre" :id "bo")]}])
                    (update-minion "bo" :damage-taken 2)
                    (get-minion "bo")
                    (:damage-taken))
                2))}
  [state id key function-or-value]
  (let [minion (get-minion state id)]
    (replace-minion state (if (fn? function-or-value)
                            (update minion key function-or-value)
                            (assoc minion key function-or-value)))))

(defn get-characters
  "Returns the characters (heroes and minions) on the board for the given player-id or for both players."
  {:test (fn []
           (is= (-> (create-game [{:hero (create-hero "Anduin Wrynn" :id "h1")}])
                    (get-characters)
                    (first)
                    (:name))
                "Anduin Wrynn")
           (is= (-> (create-game [{:hero (create-hero "Anduin Wrynn" :id "h1")}])
                    (get-characters "p1")
                    (first)
                    (:name))
                "Anduin Wrynn"))}
  ([state player-id]
   (let [minions (get-minions state player-id)
         hero (:hero (get-player state player-id))]
     (conj minions hero)))
  ([state]
   (concat (get-characters state "p1")
           (get-characters state "p2"))))

(defn get-character
  "Returns the character with the given id from the state."
  {:test (fn []
           (is= (-> (create-game [{:hero (create-hero "Anduin Wrynn" :id "h1")}])
                    (get-character "h1")
                    (:name))
                "Anduin Wrynn")
           (is= (-> (create-game [{:minions [(create-minion "Boulderfist Ogre" :id "bo")]}])
                    (get-character "bo")
                    (:name))
                "Boulderfist Ogre"))}
  [state id]
  (->> (get-characters state)
       (filter (fn [m] (= (:id m) id)))
       (first)))

(defn update-character
  {:test (fn []
           (is= (-> (create-game [{:hero (create-hero "Anduin Wrynn" :id "h")}])
                    (update-character "h" :damage-taken 2)
                    (get-player "p1")
                    (:hero)
                    (:damage-taken))
                2)
           (is= (-> (create-game [{:minions [(create-minion "Boulderfist Ogre" :id "bo")]}])
                    (update-minion "bo" :damage-taken inc)
                    (get-minion "bo")
                    (:damage-taken))
                1))}
  [state char-id key function-or-value]
  (let [character (get-character state char-id)
        entity-type (:entity-type character)]
    (if (= entity-type :minion)
      (update-minion state char-id key function-or-value)
      (update-hero state char-id key function-or-value))))

(defn update-characters
  {:test (fn []
           (is= (-> (create-game [{:hero (create-hero "Anduin Wrynn" :id "aw")
                                   :minions [(create-minion "Boulderfist Ogre" :id "bo")]}])
                    (update-characters :damage-taken inc "aw" "bo")
                    (get-character "bo")
                    (:damage-taken))
                1)
           (is= (-> (create-game [{:hero (create-hero "Anduin Wrynn" :id "aw")
                                   :minions [(create-minion "Boulderfist Ogre" :id "bo")]}])
                    (update-characters :test 1 "aw" "bo")
                    (get-character "bo")
                    (:test))
                1))}
  [state key function-or-value & ids]
  (reduce (fn [state id] (update-character state id key function-or-value)) state ids))

(defn get-deck
  {:test (fn []
           (is= (-> (create-empty-state)
                    (get-deck "p1"))
                []))}
  [state player-id]
  (get-in state [:players player-id :deck]))

(defn get-hand
  {:test (fn []
           (is= (-> (create-empty-state)
                    (get-hand "p1"))
                []))}
  [state player-id]
  (get-in state [:players player-id :hand]))

(defn get-graveyard
  {:test (fn []
           (is= (-> (create-empty-state)
                    (get-graveyard "p1"))
                []))}
  [state player-id]
  (get-in state [:players player-id :graveyard]))

(defn get-taunts
  {:test (fn []
           (is= (-> (create-empty-state)
                    (get-taunts "p1"))
                []))}
  [state player-id]
  (get-in state [:players player-id :taunts]))

(defn get-num-from-deck
  "Return the number of cards in the deck"
  {:test (fn []
           (is= (->(create-game [{:deck ["Injured Blademaster" "Silver Hand Recruit"]}])
                   (get-num-from-deck "p1"))
                2))}
  [state player-id]
  (count (get-deck state player-id)))

(defn get-num-from-hand
  "Return the number of cards in the deck"
  {:test (fn []
           (is= (->(create-game [{:hand ["Injured Blademaster" "Silver Hand Recruit"]}])
                   (get-num-from-hand "p1"))
                2))}
  [state player-id]
  (count (get-hand state player-id)))

(defn- generate-id
  "Generates an id and returns a tuple with the new state and the generated id."
  {:test (fn []
           (is= (generate-id {:counter 6})
                [{:counter 7} 6]))}
  [state]
  {:pre [(contains? state :counter)]}
  [(update state :counter inc) (:counter state)])

(defn generate-time-id
  "Generates a number and returns a tuple with the new state and the generated number."
  {:test (fn []
           (is= (generate-id {:counter 6})
                [{:counter 7} 6]))}
  [state]
  {:pre [(contains? state :counter)]}
  [(update state :counter inc) (:counter state)])

(defn assoc-event
  [state event]
  (assoc state :event event))

(defn add-card-to
  "Adds a card to either the hand or the deck."
  {:test (fn []
           ; Adding cards to deck
           (is= (as-> (create-empty-state) $
                      (add-card-to $ "p1" "Boulderfist Ogre" :deck)
                      (add-card-to $ "p1" "Injured Blademaster" :deck)
                      (get-deck $ "p1")
                      (map :name $))
                ["Boulderfist Ogre" "Injured Blademaster"])
           ; Adding cards to hand
           (is= (as-> (create-empty-state) $
                      (add-card-to $ "p1" "Boulderfist Ogre" :hand)
                      (add-card-to $ "p1" "Injured Blademaster" :hand)
                      (get-hand $ "p1")
                      (map :name $))
                ["Boulderfist Ogre" "Injured Blademaster"]))}
  [state player-id card-or-name place]
  (let [card (if (string? card-or-name)
               (create-card card-or-name)
               card-or-name)
        [state id] (if (contains? card :id)
                     [state (:id card)]
                     (let [[state value] (generate-id state)]
                       [state (str "c" value)]))
        ready-card (assoc card :owner-id player-id
                               :id id)]
    (update-in state [:players player-id place] conj ready-card)))

(defn add-card-to-deck
  [state player-id card]
  (add-card-to state player-id card :deck))

(defn add-card-to-hand
  ; TODO: Discuss this
  [state player-id card]
  (let [cast-when-drawn (:cast-when-drawn (get-definition card))]
    (if cast-when-drawn
      (cast-when-drawn state)
      (add-card-to state player-id card :hand))))

(defn add-card-to-graveyard
  "Add a minion to graveyard"
  [state player-id card]
  (add-card-to state player-id card :graveyard))

(defn add-card-to-taunts
  "Add a minion to taunts"
  [state player-id card]
  (add-card-to state player-id card :taunts))

(defn add-cards-to-deck
  [state player-id cards]
  (reduce (fn [state card]
            (add-card-to-deck state player-id card))
          state
          cards))

(defn add-cards-to-hand
  [state player-id cards]
  (reduce (fn [state card]
            (add-card-to-hand state player-id card))
          state
          cards))

(defn add-id-to-sleep
  {:test (fn []
           (is= (-> (create-game [(create-hero "Uther Lightbringer" :id "h1")
                                  {:hero (create-hero "Anduin Wrynn" :id "h2")
                                   :minions [(create-minion "Boulderfist Ogre" :id "bo")
                                             (create-minion "Unstable Ghoul" :id "ug")]}])
                    (add-id-to-sleep "bo")
                    (:minion-ids-summoned-this-turn)
                    (count))
                1))}
  [state id]
  (let [sleep (:minion-ids-summoned-this-turn state)
        updated-sleep (conj sleep id)]
    (assoc state :minion-ids-summoned-this-turn updated-sleep)))

(defn add-aura
  {:test (fn []
           (let [minion (create-minion "Dire Wolf Alpha" :id "hs")
                 ready-minion (assoc minion :position 0
                                            :owner-id "p1"
                                            :attack-gain 0
                                            :health-gain 0
                                            :added-to-board-time-id 0)
                 adjacent-minion (create-minion "Boulderfist Ogre" :id "bo")
                 ready-minion2 (assoc adjacent-minion
                                             :position 1
                                             :owner-id "p1"
                                             :attack-gain 0
                                             :health-gain 0
                                             :added-to-board-time-id 0)]
             (is= (-> (create-empty-state)
                      (assoc-in [:players "p1" :minions] [ready-minion ready-minion2])
                      (add-aura ready-minion)
                      (get-minion "bo")
                      (:attack-gain))
                  1)))}
  [state ready-minion]
  (let [aura-filters (get-definition "aura-filters")
        card-def (get-definition ready-minion)
        aura (:aura card-def)
        aura-filter ((:filter aura) aura-filters)
        extra-inputs {:minion-id   (:id ready-minion)
                      :player-id   (:owner-id ready-minion)}
        aura-characters (aura-filter state extra-inputs)
        aura-character-ids (map :id aura-characters)
        aura-key-vals (dissoc aura :filter)]
    (reduce (fn [state [key val]]
                (apply update-characters state key (fn [x] (+ x val)) aura-character-ids)) state aura-key-vals)))

(defn remove-aura
  ; TODO: Can a minion die when an aura gets removed?
  {:test (fn []
           (let [minion (create-minion "Dire Wolf Alpha" :id "hs")
                 ready-minion (assoc minion :position 0
                                            :owner-id "p1"
                                            :attack-gain 0
                                            :health-gain 0
                                            :added-to-board-time-id 0)
                 adjacent-minion (create-minion "Boulderfist Ogre" :id "bo")
                 ready-minion2 (assoc adjacent-minion
                                 :position 1
                                 :owner-id "p1"
                                 :attack-gain 0
                                 :health-gain 0
                                 :added-to-board-time-id 0)]
             (is= (-> (create-empty-state)
                      (assoc-in [:players "p1" :minions] [ready-minion ready-minion2])
                      (add-aura ready-minion)
                      (remove-aura ready-minion)
                      (get-minion "bo")
                      (:attack-gain))
                  0)))}
  [state minion]
  (let [aura-filters (get-definition "aura-filters")
        card-def (get-definition minion)
        aura (:aura card-def)
        aura-filter ((:filter aura) aura-filters)
        extra-inputs {:minion-id   (:id minion)
                      :player-id   (:owner-id minion)}
        aura-characters (aura-filter state extra-inputs)
        aura-character-ids (map :id aura-characters)
        aura-key-vals (dissoc aura :filter)]
    (reduce (fn [state [key val]]
              (apply update-characters state key (fn [x] (- x val)) aura-character-ids)) state aura-key-vals)))

(defn add-minion-to-board
  "Adds a minion with a given position to a player's minions and updates the other minions' positions."
  {:test (fn []
           ; Adding a minion to an empty board
           (is= (as-> (create-empty-state) $
                      (add-minion-to-board $ "p1" (create-minion "Boulderfist Ogre" :id "bo") 0)
                      (get-minions $ "p1")
                      (map (fn [m] {:id (:id m) :name (:name m)}) $))
                [{:id "bo" :name "Boulderfist Ogre"}])
           ; Adding a minion and update positions
           (let [minions (-> (create-empty-state)
                             (add-minion-to-board "p1" (create-minion "Boulderfist Ogre" :id "bo1") 0)
                             (add-minion-to-board "p1" (create-minion "Boulderfist Ogre" :id "bo2") 0)
                             (add-minion-to-board "p1" (create-minion "Boulderfist Ogre" :id "bo3") 1)
                             (get-minions "p1"))]
             (is= (map :id minions) ["bo1" "bo2" "bo3"])
             (is= (map :position minions) [2 0 1]))
           ; Generating an id for the new minion
           (let [state (-> (create-empty-state)
                           (add-minion-to-board "p1" (create-minion "Boulderfist Ogre") 0))]
             (is= (-> (get-minions state "p1")
                      (first)
                      (:name))
                  "Boulderfist Ogre")
             (is= (:counter state) 3)))}
  [state player-id minion position]
  {:pre [(map? state) (string? player-id) (map? minion) (number? position)]}
  (let [[state id] (if (contains? minion :id)
                     [state (:id minion)]
                     (let [[state value] (generate-id state)]
                       [state (str "m" value)]))
        [state time-id] (generate-time-id state)
        card-def (get-definition minion)
        ready-minion (assoc minion :position position
                                   :owner-id player-id
                                   :id id
                                   :attack-gain 0
                                   :health-gain 0
                                   :added-to-board-time-id time-id
                                   :cannot-attack (:cannot-attack card-def))]
    (cond-> state
            (:taunt card-def) (add-card-to-taunts player-id ready-minion)
            true              (do-on-summons player-id)
            true              (update-in [:players player-id :minions]
                                   (fn [minions]
                                     (conj (->> minions
                                                (mapv (fn [m]
                                                        (if (< (:position m) position)
                                                          m
                                                          (update m :position inc)))))
                                           ready-minion)))
            (:aura card-def)  (add-aura ready-minion))))

(defn has-minion-in-board?
  "Check if there is the minion in the board"
  {:test (fn []
           (is (-> (create-game [{:minions ["Houndmaster Shaw"]}])
                   (has-minion-in-board? "p1" "Houndmaster Shaw"))))}
  [state player-id minion-name]
  (as-> (get-minions state player-id) $
        (map :name $)
        (.contains $ minion-name)))

(defn add-minion-to-board-and-sleep
  "Return state after adding minion to board and minion-ids-summoned-this-turn"
  {:test (fn []
           ; The number of ids in minion-ids-summoned-this-turn should be 3 after adding 3 minions
           (is= (-> (create-empty-state)
                    (add-minion-to-board-and-sleep "p1" (create-minion "Boulderfist Ogre") 0)
                    (add-minion-to-board-and-sleep "p1" (create-minion "Boulderfist Ogre") 0)
                    (add-minion-to-board-and-sleep "p1" (create-minion "Boulderfist Ogre") 1)
                    (:minion-ids-summoned-this-turn)
                    (count))
                3))}
  [state player-id minion position]
  {:pre [(map? state) (string? player-id) (map? minion) (number? position)]}
  (let [[state id] (if (contains? minion :id)
                     [state (:id minion)]
                     (let [[state value] (generate-id state)]
                       [state (str "m" value)]))
        [state time-id] (generate-time-id state)
        card-def (get-definition minion)
        ready-minion (assoc minion :position position
                                   :owner-id player-id
                                   :id id
                                   :attack-gain 0
                                   :health-gain 0
                                   :added-to-board-time-id time-id
                                   :cannot-attack (:cannot-attack card-def))]
    (cond-> state
            (:taunt card-def) (add-card-to-taunts player-id ready-minion)
            true              (do-on-summons player-id)
            true              (update-in [:players player-id :minions]
                                         (fn [minions]
                                           (conj (->> minions
                                                      (mapv (fn [m]
                                                              (if (< (:position m) position)
                                                                m
                                                                (update m :position inc)))))
                                                 ready-minion)))
            (:aura card-def)  (add-aura ready-minion)
            (not (has-minion-in-board? state player-id "Houndmaster Shaw"))
              (add-id-to-sleep id))))

(defn add-minions-to-board
  {:test (fn []
           (is= (as-> (create-empty-state) $
                      (add-minions-to-board $ "p1" [(create-minion "Boulderfist Ogre")
                                                    "Silver Hand Recruit"
                                                    (create-minion "Injured Blademaster")])
                      (get-minions $ "p1")
                      (map :name $))
                ["Boulderfist Ogre" "Silver Hand Recruit" "Injured Blademaster"]))}
  [state player-id minions]
  (->> minions
       (reduce-kv (fn [state index minion]
                    (add-minion-to-board state
                                         player-id
                                         (if (string? minion)
                                           (create-minion minion)
                                           minion)
                                         index))
                  state)))

(defn update-mana
  {:test (fn []
           (is= (-> (create-empty-state)
                    (update-mana "p1" 5)
                    (get-in [:players "p1" :mana]))
                5)
           (is= (-> (create-empty-state)
                    (update-mana "p1" (fn [x] (- x 3)))
                    (get-in [:players "p1" :mana]))
                7))}
  [state player-id amount-or-update-fn]
  (if (fn? amount-or-update-fn)
    (update-in state [:players player-id :mana] amount-or-update-fn)
    (assoc-in state [:players player-id :mana] amount-or-update-fn)))

(defn create-game
  "Creates a game with the given deck, hand, minions (placed on the board), and heroes."
  {:test (fn []
           (is= (create-game) (create-empty-state))

           (is= (create-game [{:hero (create-hero "Anduin Wrynn")}])
                (create-game [{:hero "Anduin Wrynn"}]))

           (is= (create-game [{:minions [(create-minion "Boulderfist Ogre")]}])
                (create-game [{:minions ["Boulderfist Ogre"]}]))

           (is= (create-game [{:minions ["Boulderfist Ogre"]
                               :deck    ["Injured Blademaster"]
                               :hand    ["Silver Hand Recruit"]}
                              {:hero "Anduin Wrynn"}]
                             :player-id-in-turn "p2")
                {:player-id-in-turn             "p2"
                 :players                       {"p1" {:id      "p1"
                                                       :deck    [{:entity-type :card
                                                                  :id          "c3"
                                                                  :name        "Injured Blademaster"
                                                                  :owner-id    "p1"}]
                                                       :hand    [{:entity-type :card
                                                                  :id          "c4"
                                                                  :name        "Silver Hand Recruit"
                                                                  :owner-id    "p1"}]
                                                       :minions [{:damage-taken                0
                                                                  :attacks-performed-this-turn 0
                                                                  :added-to-board-time-id      2
                                                                  :entity-type                 :minion
                                                                  :name                        "Boulderfist Ogre"
                                                                  :id                          "m1"
                                                                  :attack-gain                 0
                                                                  :health-gain                 0
                                                                  :position                    0
                                                                  :cannot-attack              nil
                                                                  :owner-id                    "p1"}]
                                                       :mana    10
                                                       :fatigue-counter 0
                                                       :graveyard []
                                                       :taunts  []
                                                       :hero    {:name         "Uther Lightbringer"
                                                                 :id           "h1"
                                                                 :entity-type  :hero
                                                                 :damage-taken 0
                                                                 :hero-power-available true
                                                                 :hero-power "Reinforce"
                                                                 :attacks-performed-this-turn 0
                                                                 :health-gain  0}}
                                                 "p2" {:id      "p2"
                                                       :deck    []
                                                       :hand    []
                                                       
                                                       
                                                       :minions []
                                                       :mana    10
                                                       :fatigue-counter 0
                                                       :graveyard []
                                                       :taunts  []
                                                       :hero    {:name         "Anduin Wrynn"
                                                                 :id           "h2"
                                                                 :entity-type  :hero
                                                                 :damage-taken 0
                                                                 :hero-power-available true
                                                                 :hero-power "Lesser Heal"
                                                                 :attacks-performed-this-turn 0
                                                                 :health-gain  0}}}
                 :counter                       5
                 :minion-ids-summoned-this-turn []
                 :action-index                  0
                 :event                         {:name           "start-of-game"}
                 :seed 888}))}
  ([data & kvs]
   (let [players-data (map-indexed (fn [index player-data]
                                     (assoc player-data :player-id (str "p" (inc index))))
                                   data)
         state (as-> (create-empty-state (map (fn [player-data]
                                                (cond (nil? (:hero player-data))
                                                      (create-hero "Uther Lightbringer")

                                                      (string? (:hero player-data))
                                                      (create-hero (:hero player-data))

                                                      :else
                                                      (:hero player-data)))
                                              data)) $
                     (reduce (fn [state {player-id :player-id
                                         minions   :minions
                                         deck      :deck
                                         hand      :hand}]
                               (-> state
                                   (add-minions-to-board player-id minions)
                                   (add-cards-to-deck player-id deck)
                                   (add-cards-to-hand player-id hand)))
                             $
                             players-data))]
     (if (empty? kvs)
       state
       (apply assoc state kvs))))
  ([]
   (create-game [])))

(defn get-mana
  {:test (fn []
           (is= (-> (create-game)
                    (get-mana "p1"))
                10)
           (is= (-> (create-game [])
                    (update-mana "p1" (fn [x] (- x 2)))
                    (get-mana "p1"))
                8))}
  [state player-id]
  (let [player (get-player state player-id)]
    (:mana player)))

(defn get-card-from-hand
  {:test (fn []
           (-> (create-game [{:hand [(create-card "Boulderfist Ogre" :id "bo")]}])
               (get-card-from-hand "bo")
               (:name)
               (is= "Boulderfist Ogre")))}
  [state id]
  (->> (get-players state)
       (map :hand)
       (apply concat)
       (filter (fn [c] (= (:id c) id)))
       (first)))

(defn get-owner
  "Returns the owner id of the character"
  {:test (fn []
           (is= (-> (create-game [(create-hero "Uther Lightbringer" :id "h1")
                                  {:hero (create-hero "Anduin Wrynn" :id "h2")
                                   :minions [(create-minion "Boulderfist Ogre" :id "bo")
                                             (create-minion "Unstable Ghoul" :id "ug")]}])
                    (get-owner "h1"))
                "p1")
           (is= (-> (create-game [(create-hero "Uther Lightbringer" :id "h1")
                                  {:hero (create-hero "Anduin Wrynn" :id "h2")
                                   :minions [(create-minion "Boulderfist Ogre" :id "bo")
                                             (create-minion "Unstable Ghoul" :id "ug")]}])
                    (get-owner "h2"))
                "p2")
           (is= (-> (create-game [(create-hero "Uther Lightbringer" :id "h1")
                                  {:hero (create-hero "Anduin Wrynn" :id "h2")
                                   :minions [(create-minion "Boulderfist Ogre" :id "bo")
                                             (create-minion "Unstable Ghoul" :id "ug")]}])
                    (get-owner "bo"))
                "p2"))}
  [state id]
  (let [character (get-character state id)
        type (:entity-type character)]
    (if (= type :hero)
      (if (= (get-in state [:players "p1" :hero :id]) id)
        "p1"
        "p2")
      (:owner-id character))))

(defn get-damage-taken
  "Return damage-taken of a character"
  {:test (fn []
           (is= (-> (create-game [(create-hero "Uther Lightbringer" :id "h1")
                                  {:hero (create-hero "Anduin Wrynn" :id "h2" :damage-taken 5)
                                   :minions [(create-minion "Boulderfist Ogre" :id "bo" :damage-taken 1)
                                             (create-minion "Unstable Ghoul" :id "ug")]}])
                    (get-damage-taken "bo"))
                1)
           (is= (-> (create-game [(create-hero "Uther Lightbringer" :id "h1")
                                  {:hero (create-hero "Anduin Wrynn" :id "h2" :damage-taken 5)
                                   :minions [(create-minion "Boulderfist Ogre" :id "bo" :damage-taken 1)
                                             (create-minion "Unstable Ghoul" :id "ug")]}])
                    (get-damage-taken "h2"))
                5))}
  [state id]
  (:damage-taken (get-character state id)))

(defn reset-hero-power
  {:test (fn []
           (is= (-> (create-empty-state)
                    (reset-hero-power "p1")
                    (get-player "p1")
                    (:hero)
                    (:hero-power-available)) true))}
  [state player-id]
  (assoc-in state [:players player-id :hero :hero-power-available] true))

(defn do-deathrattle
  [state minion-id]
  (maybe-do-minion-ability state minion-id :deathrattle))

(defn remove-minion
  "Removes a minion with the given id from the state."
  {:test (fn []
           (is= (-> (create-game [{:minions [(create-minion "Boulderfist Ogre" :id "bo")]}])
                    (remove-minion "bo")
                    (get-minions))
                [])
           (is= (-> (create-game [{:minions [(create-minion "Boulderfist Ogre" :id "bo")]}])
                    (remove-minion "bo")
                    (get-graveyard "p1")
                    (nth 0)
                    (:name))
                "Boulderfist Ogre")
           (is= (-> (create-game [{:minions [(create-minion "Leper Gnome" :id "lg")]}])
                    (remove-minion "lg")
                    (get-damage-taken "h2"))
                2))}
  [state id]
  (let [minion (get-minion state id)
        card-def (get-definition minion)
        owner-id (:owner-id minion)
        minions (get-minions state owner-id)
        position (:position minion)]

    (cond-> state
            (:aura card-def)        (remove-aura minion)
            minions                 (assoc-in [:players owner-id :minions] (map (fn [p]
                                                                                     (if (> (:position p) position)
                                                                                       (update p :position dec)
                                                                                       p))
                                                                                   minions))
            true                    (-> (do-deathrattle id)
                                        (add-card-to-graveyard owner-id minion)
                                        (update-in [:players owner-id :minions]
                                                    (fn [minions] (remove (fn [m] (= (:id m) id)) minions)))
                                        (update-in [:players owner-id :taunts]
                                                   (fn [taunts] (remove (fn [m] (= (:id m) id)) taunts)))))))

(defn remove-minion-from-deck
  "Removes a minion with the given id from the deck."
  {:test (fn []
           (is= (-> (create-game [{:deck [(create-minion "Boulderfist Ogre" :id "bo")]}])
                    (remove-minion-from-deck "p1" "bo")
                    (get-deck "p1"))
                [])
           (is= (as-> (create-game [{:deck [(create-minion "Boulderfist Ogre" :id "bo")]}]) $
                    (remove-minion-from-deck $ "p1" "bo1")
                    (get-deck $ "p1")
                      (map :id $))
                ["bo"]))}
  [state player-id id]
  (update-in state
             [:players player-id :deck]
             (fn [deck]
               (remove (fn [m] (= (:id m) id)) deck))))

(defn remove-minion-from-graveyard
  "Removes a minion with the given id from the graveyard."
  {:test (fn []
           (is= (-> (create-game [{:minions [(create-minion "Boulderfist Ogre" :id "bo")]}])
                    (remove-minion "bo")
                    (remove-minion-from-graveyard "p1" "bo")
                    (get-graveyard "p1"))
                []))}
  [state player-id id]
  (update-in state
             [:players player-id :graveyard]
             (fn [graveyard]
               (remove (fn [m] (= (:id m) id)) graveyard))))

(defn remove-minion-from-hand
  "Removes a minion with the given id from the hand."
  {:test (fn []
           (is= (-> (create-game [{:hand [(create-minion "Boulderfist Ogre" :id "bo")]}])
                    (remove-minion-from-hand "p1" "bo")
                    (get-hand "p1"))
                []))}
  [state player-id id]
  (update-in state
             [:players player-id :hand]
             (fn [hand]
               (remove (fn [m] (= (:id m) id)) hand))))

(defn remove-minions
  "Removes the minions with the given ids from the state."
  {:test (fn []
           (is= (as-> (create-game [{:minions [(create-minion "Boulderfist Ogre" :id "bo1")
                                               (create-minion "Boulderfist Ogre" :id "bo2")]}
                                    {:minions [(create-minion "Boulderfist Ogre" :id "bo3")
                                               (create-minion "Boulderfist Ogre" :id "bo4")]}]) $
                      (remove-minions $ "bo1" "bo4")
                      (get-minions $)
                      (map :id $))
                ["bo2" "bo3"]))}
  [state & ids]
  (reduce remove-minion state ids))

; Functions that utilize randomness:

(defn get-random-int-and-update-seed
  [state max]
  (let [rand (get-random-int (:seed state) max)]
    [(assoc state :seed (first rand)) (last rand)]))

(defn get-random-element
  {:test (fn []
           (let [state (create-game [{:minions [(create-minion "Lowly Squire" :id "ls1")
                                                (create-minion "Lowly Squire" :id "ls2")]}])
                 characters (get-characters state)]
             (is
               (as-> (get-random-element state characters) $
                     (last $)
                     (:id $)
                     (or
                       (= $ "ls1")
                       (= $ "ls2")
                       (= $ "h1")
                       (= $ "h2")))))
           (let [state (create-game )
                 minions (get-characters state)]))}
  [state collection]
  (if (seq collection)
    (let [max (count collection)
          [state rand-int] (get-random-int-and-update-seed state max)
          rand-element (nth collection rand-int)]
      [state rand-element])
    [state nil]))

(defn get-random-card-from-graveyard
  "Return a random card from the graveyard"
  {:test (fn []
           (is= (.contains (as-> (create-game [{:minions [(create-minion "Boulderfist Ogre" :id "bo")
                                                     (create-minion "Injured Blademaster" :id "b1")]}]) $
                            (remove-minions $ "bo" "b1") $
                            (get-graveyard $ "p1"))
                      (as-> (create-game [{:minions [(create-minion "Boulderfist Ogre" :id "bo")
                                                     (create-minion "Injured Blademaster" :id "b1")]}]) $
                            (remove-minions $ "bo" "b1") $
                            (get-random-card-from-graveyard $ "p1")
                            (last $))
                      )
                true))}
  [state player-id]
  (let [graveyard (get-graveyard state player-id)]
    (get-random-element state graveyard)))

(defn get-random-card-from-taunts
  "Return a random card from the taunts"
  {:test (fn []
           (is= (.contains (as-> (create-game []) $
                            (add-minions-to-board $ "p1" [(create-minion "Unstable Ghoul" :id "ug1")
                                                          (create-minion "Unstable Ghoul" :id "ug2")])
                            (get-taunts $ "p1"))
                      (-> (create-game [])
                            (add-minions-to-board "p1" [(create-minion "Unstable Ghoul" :id "ug1")
                                                          (create-minion "Unstable Ghoul" :id "ug2")])
                            (get-random-card-from-taunts "p1")
                            (last))
                      )
                true))}
  [state player-id]
  (let [taunts (get-taunts state player-id)]
    (get-random-element state taunts)))

(defn get-random-card-from-deck
  "Return a random card from the deck"
  {:test (fn []
           (is= (.contains (as-> (create-game [{:deck ["Injured Blademaster" "Silver Hand Recruit"]}]) $
                                 (get-deck $ "p1"))
                           (as-> (create-game [{:deck ["Injured Blademaster" "Silver Hand Recruit"]}]) $
                                 (get-random-card-from-deck $ "p1")
                                 (last $))
                           )
                true)
           (is-not (-> (create-game [])
                       (get-random-card-from-deck "p1")
                       (last))))}
  [state player-id]
  (get-random-element state (get-deck state player-id)))

(defn get-random-minion
  "Return a minion card from the board"
  {:test (fn []
           (is= (.contains (as-> (create-game [{:minions ["Injured Blademaster" "Silver Hand Recruit"]}]) $
                                 (get-minions $ "p1"))
                           (as-> (create-game [{:minions ["Injured Blademaster" "Silver Hand Recruit"]}]) $
                                 (get-random-minion $ "p1")
                                 (last $))
                           )
                true)
           (is (as-> (create-game [{:minions ["Injured Blademaster" "Silver Hand Recruit"]}
                                   {:minions ["Boulderfist Ogre"]}]) $
                     (get-random-minion $)
                     (last $)
                     (:name $)
                     (or (= $ "Injured Blademaster")
                         (= $ "Silver Hand Recruit")
                         (= $ "Boulderfist Ogre")))))}
  ([state player-id]
   (get-random-element state (get-minions state player-id)))
  ([state]
   (get-random-element state (get-minions state))))

(defn get-random-character
  "Return a random character(minion or hero) from a player."
  {:test (fn []
           (let [state (create-game [{:minions [(create-minion "Lowly Squire" :id "ls1")
                                                (create-minion "Lowly Squire" :id "ls2")]}]
                                    :player-id-in-turn "p1")]
             (is
               (as-> (get-random-character state "p1") $
                     (last $)
                     (:id $)
                     (or
                       (= $ "ls1")
                       (= $ "ls2")
                       (= $ "h1"))))
             (is
               (as-> (get-random-character state) $
                     (last $)
                     (:id $)
                     (or
                       (= $ "ls1")
                       (= $ "ls2")
                       (= $ "h1")
                       (= $ "h2"))))))}
  ([state player-id]
   (get-random-element state (get-characters state player-id)))
  ([state]
   (get-random-element state (get-characters state))))



(defn update-characters-perform
  "Return the state after update a players' characters' attacks-performed-this-turn"
  {:test (fn []
           (let [characters (-> (create-game [(create-hero "Uther Lightbringer" :id "h1")
                                            {:hero (create-hero "Anduin Wrynn" :id "h2")
                                             :minions [(create-minion "Boulderfist Ogre" :id "bo")
                                                       (create-minion "Unstable Ghoul" :id "ug")]}])
                                (update-characters-perform "p2" 1)
                                (get-characters "p2"))
                 performs (map :attacks-performed-this-turn characters)]
             (is (every? (fn [p] (= p 1)) performs))))}
  [state player-id value]
  (let [minions (get-minions state player-id)
        updated-minions (map (fn [p]
                               (assoc p :attacks-performed-this-turn value)) minions)
        hero (get-hero state player-id)
        updated-hero (assoc hero :attacks-performed-this-turn value)]
    (-> (assoc-in state [:players player-id :minions] updated-minions)
        (assoc-in [:players player-id :hero] updated-hero))))

(defn update-hero-power
  "Return the state after the hero power is refilled"
  {:test (fn []
           (is=(-> (create-game)
               (assoc-in [:players "p1" :hero :hero-power-available] false)
               (update-hero-power "p1")
               (get-in [:players "p1" :hero :hero-power-available]))
               true))}
  [state player-id]
  (assoc-in state [:players player-id :hero :hero-power-available] true))