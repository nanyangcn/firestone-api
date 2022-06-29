(ns firestone.core-api
  (:require [ysera.test :refer [is is-not is= error?]]
            [ysera.error :refer [error]]
            [firestone.construct :refer :all]
            [firestone.core :refer :all]
            [firestone.definitions :refer :all]))

(defn do-on-spell-casts
  {:test (fn []
           (-> (create-game [{:minions ["Archmage Antonidas"]
                              :deck    ["Injured Blademaster"]
                              :hand    ["Sprint"]}] )
               (do-on-spell-casts  "p1")
               (get-hand "p1")
               (last)
               (:name)
               (is= "Fireball")))}
  [state player-id]
  (maybe-do-minion-abilities state player-id :on-spell-cast))

(defn do-battlecry
  [state minion-id & {:keys [target-id]}]
  (maybe-do-minion-ability state minion-id :battlecry :target-id target-id))

(defn do-inspire
  [state minion-id & {:keys [target-id]}]
  (maybe-do-minion-ability state minion-id :inspire :target-id target-id))

(defn do-spell
  [state card-id & {:keys [target-id]}]
  (let [card                (get-card-from-hand state card-id)
        player-id           (:owner-id card)
        spell               (-> (get-definition card) (:spell))
        player-change-fn    {"p1" "p2"
                             "p2" "p1"}
        opponent-id         (player-change-fn player-id)
        next-position       0
        mana                (get-mana state player-id)
        definition          (get-definition card)
        mana-cost           (or (:mana-cost definition) 0)
        extra-inputs {:target-id   target-id
                      :player-id   player-id
                      :opponent-id opponent-id
                      :next-position next-position}]
    (if (> mana-cost mana)
      state
      (as-> (do-on-spell-casts state player-id) $
            (update $ :action-index inc)
            (pay-mana $ player-id card)
            (remove-minion-from-hand $ player-id card-id)
            (spell $ extra-inputs)
            (assoc-event $ {:name "after-spell-card-played"
                            :card card})))))

(defn do-inspires
  {:test (fn []
           (let [state (-> (create-game [{:minions [(create-minion "Lowly Squire" :id "ls1")
                                        (create-minion "Lowly Squire" :id "ls2")]}])
               (do-inspires "p1"))]
             (and
               (is= (get-attack state "ls1") 2)
               (is= (get-attack state "ls2") 2))))}
  [state player-id]
  (maybe-do-minion-abilities state player-id :inspire))

(defn do-end-turn
  [state minion-id]
  (maybe-do-minion-ability state minion-id :end-turn))

(defn do-hero-power
  [state player-id & {:keys [target-id]}]
  (let [hero            (get-hero state player-id)
        mana            (:mana (get-player state player-id))
        hero-power      (:hero-power hero)
        hero-power-def  (get-definition hero-power)
        hero-power-func (:hero-power-function hero-power-def)
        mana-cost       (:mana-cost hero-power-def)
        extra-inputs    {:target-id   target-id
                         :player-id   player-id}]
    (if (< mana mana-cost)
      state
      (as-> (do-inspires state player-id) $
            (pay-mana $ player-id hero-power)
            (update $ :action-index inc)
            (hero-power-func $ extra-inputs)
            (assoc-in $ [:players player-id :hero :hero-power-available] false)
            (assoc-event $ {:name "after-hero-power-used"})))))

(defn do-end-turns
  [state player-id]
  (maybe-do-minion-abilities state player-id :end-turn))

(defn get-fatigue-damage
  "Return the state after getting fatigue damage"
  {:test (fn []
           ;Should got right damage-taken
           (is= (->(create-game)
                   (get-fatigue-damage "p1")
                   (get-in [:players "p1" :hero :damage-taken]))
                1)
           ;Should count fatigue rounds
           (is= (->(create-game)
                   (get-fatigue-damage "p1")
                   (get-in [:players "p1" :fatigue-counter]))
                1)
          ;Should got right damage-taken
          (is= (->(create-game)
                  (get-fatigue-damage "p1")
                  (get-fatigue-damage "p1")
                  (get-in [:players "p1" :hero :damage-taken]))
               3)
          ;Should count fatigue rounds
          (is= (->(create-game)
                  (get-fatigue-damage "p1")
                  (get-fatigue-damage "p1")
                  (get-in [:players "p1" :fatigue-counter]))
               2)
          )}
  [state player-id]
  (as-> (update-in state [:players player-id :fatigue-counter] + 1) $
      (update-in $ [:players player-id :hero :damage-taken] + (get-fatigue-counter $ player-id))))

(defn draw-card
  "Return state after a player drawing a card from deck to hand"
  {:test (fn []
           ; Should add a card in the hand after drawing
           (is= (-> (create-game [{:minions ["Boulderfist Ogre"]
                                   :deck    ["Injured Blademaster" "Silver Hand Recruit"]
                                   :hand    ["Silver Hand Recruit"]}
                                  {:hero "Anduin Wrynn"}]
                                 :player-id-in-turn "p1")
                    (draw-card "p1")
                    (get-in [:players "p1" :hand])
                    (count))
                2)
           (is= (-> (create-game [{:minions ["Boulderfist Ogre"]
                                   :deck    ["Injured Blademaster" "Silver Hand Recruit"]
                                   :hand    ["Silver Hand Recruit" "Silver Hand Recruit" "Silver Hand Recruit" "Silver Hand Recruit" "Silver Hand Recruit" "Silver Hand Recruit" "Silver Hand Recruit" "Silver Hand Recruit" "Silver Hand Recruit" "Silver Hand Recruit"]}
                                  {:hero "Anduin Wrynn"}]
                                 :player-id-in-turn "p1")
                    (draw-card "p1")
                    (get-in [:players "p1" :hand])
                    (count))
                10)
           ;Should remove a card in the deck after drawing
           (is= (-> (create-game [{:minions ["Boulderfist Ogre"]
                                   :deck    ["Injured Blademaster" "Silver Hand Recruit"]
                                   :hand    ["Silver Hand Recruit"]}
                                  {:hero "Anduin Wrynn"}]
                                 :player-id-in-turn "p1")
                    (draw-card "p1")
                    (get-in [:players "p1" :deck])
                    (count))
                1))}
  [state player-id]
  (as-> state $
        (if (fatigue? $ player-id)
          (get-fatigue-damage $ player-id)
          (let [[state random-card] (get-random-card-from-deck $ player-id)]
            (if random-card
              (cond-> (remove-minion-from-deck state player-id (:id random-card))
                      (< (get-num-from-hand state player-id) 10) (add-card-to-hand player-id random-card)
                      true (assoc-event {:name "card-drawn"
                                         :card random-card}))
              $)))))

(defn draw-cards
  {:test (fn []
           (is= (-> (create-game [{:minions ["Boulderfist Ogre"]
                                   :deck    ["Injured Blademaster" "Silver Hand Recruit"]
                                   :hand    ["Silver Hand Recruit"]}
                                  {:hero "Anduin Wrynn"}]
                                 :player-id-in-turn "p1")
                    (draw-cards "p1" 2)
                    (get-in [:players "p1" :hand])
                    (count))
                3)
           (is= (-> (create-game [{:minions ["Boulderfist Ogre"]
                                   :deck    ["Injured Blademaster" "Silver Hand Recruit"]
                                   :hand    ["Silver Hand Recruit"]}
                                  {:hero "Anduin Wrynn"}]
                                 :player-id-in-turn "p1")
                    (draw-cards "p1" 10)
                    (get-in [:players "p1" :hand])
                    (count))
                3)
           ; Should get fatigue damage
           (is= (-> (create-game [{:minions ["Boulderfist Ogre"]
                                   :deck    ["Injured Blademaster" "Silver Hand Recruit"]
                                   :hand    ["Silver Hand Recruit"]}
                                  {:hero "Anduin Wrynn"}]
                                 :player-id-in-turn "p1")
                    (draw-cards "p1" 10)
                    (get-in [:players "p1" :hero :damage-taken]))
                36))}
  [state player-id num]
  (reduce draw-card state (vec (repeat num player-id))))

(defn play-minion-card
  {:test (fn []
           (let [player-id "p1"
                 state (-> (create-game [{:minions ["Boulderfist Ogre"]
                                          :deck    ["Silver Hand Recruit", "Silver Hand Recruit"]
                                          :hand    [(create-minion "The Black Knight" :id "bk")]}
                                         {:hero "Anduin Wrynn"}]
                                        :player-id-in-turn "p1")
                           (play-minion-card player-id "bk" 0))
                 player-mana (:mana (get-player state player-id))]
             (is= (get-hand state "p1") [])
             (is= (count (get-minions state "p1")) 2)))}
  [state player-id card-id position & {target-id :target-id}]
  (let [card (get-card-from-hand state card-id)
        card-name (:name card)
        owner-id (:owner-id card)
        player-mana ((get-player state player-id) :mana)
        card-def (get-definition card-name)
        mana-cost (or (card-def :mana-cost) 0)
        num-minions (count (get-in state [:players player-id :minions]))
        minion (create-minion card-name :id card-id)]
    (if (not (= owner-id player-id))
      state
      (if (< player-mana mana-cost)
        state
        (if (>= num-minions 7)
          state
          (-> (add-minion-to-board-and-sleep state player-id minion position)
              (update :action-index inc)
              (remove-minion-from-hand player-id card-id)
              (pay-mana player-id card)
              (do-battlecry card-id :target-id target-id)
              (assoc-event {:name "after-minion-card-played"
                            :card card})))))))

(defn end-turn
  "Return state after a player end his turn"
  {:test (fn []
           ;Should exchange turn after ending turn
           (is= (-> (create-game)
                    (end-turn "p1")
                    (get-player-id-in-turn))
                "p2")
           ;Should exchange back after ending turns twice
           (is= (-> (create-game)
                    (end-turn "p1")
                    (end-turn "p2")
                    (get-player-id-in-turn))
                "p1")
           ;Should got right damage-taken after ending turns
           (is= (-> (create-game [{:minions ["Boulderfist Ogre"]
                                   :deck    ["Injured Blademaster"]
                                   :hand    ["Silver Hand Recruit"]}
                                  {:hero "Anduin Wrynn"}]
                                 :player-id-in-turn "p2")
                    (end-turn "p2")
                    (end-turn "p1")
                    (get-in [:players "p2" :hero :damage-taken]))
                1)
           ;Should got right damage-taken after ending turn
           (is= (-> (create-game [{:minions ["Boulderfist Ogre"]
                                   :deck    ["Injured Blademaster"]
                                   :hand    ["Silver Hand Recruit"]}
                                  {:hero "Anduin Wrynn"}]
                                 :player-id-in-turn "p2")
                    (end-turn "p2")
                    (end-turn "p1")
                    (get-in [:players "p1" :hero :damage-taken]))
                0)
           ;Should count fatigue rounds after ending turn
           (is= (-> (create-game [{:minions ["Boulderfist Ogre"]
                                   :deck    ["Injured Blademaster"]
                                   :hand    ["Silver Hand Recruit"]}
                                  {:hero "Anduin Wrynn"}]
                                 :player-id-in-turn "p2")
                    (end-turn "p2")
                    (end-turn "p1")
                    (get-in [:players "p2" :fatigue-counter]))
                1)
           ;Should count fatigue rounds after ending turn
           (is= (-> (create-game [{:minions ["Boulderfist Ogre"]
                                   :deck    ["Injured Blademaster"]
                                   :hand    ["Silver Hand Recruit"]}
                                  {:hero "Anduin Wrynn"}]
                                 :player-id-in-turn "p2")
                    (end-turn "p2")
                    (end-turn "p1")
                    (get-in [:players "p1" :fatigue-counter]))
                0)
           ;Should add a card in hand after a player ending turn
           (is= (-> (create-game [{:minions ["Boulderfist Ogre"]
                                   :deck    ["Injured Blademaster"]
                                   :hand    ["Silver Hand Recruit"]}
                                  {:hero "Anduin Wrynn"}]
                                 :player-id-in-turn "p2")
                    (end-turn "p2")
                    (get-in [:players "p1" :hand])
                    (count))
                2)
           ;Should do-end-turns works well
           (let [state (-> (create-game [{:minions [(create-minion "Ragnaros the Firelord" :id "rf1")
                                                    (create-minion "Ragnaros the Firelord" :id "rf2")]}]
                                        :player-id-in-turn "p1")
                           (end-turn "p1"))]
             (is= (get-damage-taken state "h2")
                  17))
           ;Should remove a card in deck after a player ending turn
           (is= (-> (create-game [{:minions ["Boulderfist Ogre"]
                                   :deck    ["Injured Blademaster"]
                                   :hand    ["Silver Hand Recruit"]}
                                  {:hero "Anduin Wrynn"}]
                                 :player-id-in-turn "p2")
                    (end-turn "p2")
                    (get-in [:players "p1" :deck])
                    (count))
                0)
           (error? (-> (create-game)
                       (end-turn "p2"))))}
  [state player-id]
  (when-not (= (get-player-id-in-turn state) player-id)
    (error "The player with id " player-id " is not in turn."))
  (let [player-change-fn {"p1" "p2"
                          "p2" "p1"}
        opponent-player-id (player-change-fn player-id)]
    (cond-> (do-end-turns state player-id)
            true (update :action-index inc)
            true (update-mana opponent-player-id 10)
            true (update-characters-perform opponent-player-id 0)
            true (update-hero-power opponent-player-id)
            true (assoc :minion-ids-summoned-this-turn [])
            (fatigue? state opponent-player-id) (get-fatigue-damage opponent-player-id)
            (not (hero-power-available? state opponent-player-id)) (reset-hero-power opponent-player-id)
            true (update :player-id-in-turn player-change-fn)
            (not (fatigue? state opponent-player-id)) (draw-card opponent-player-id))))

(defn attack
  {:test (fn []
           (is= (-> (create-game [{:minions [(create-minion "The Black Knight" :id "bk")]}
                             {:hero "Anduin Wrynn"
                              :minions [(create-minion "Boulderfist Ogre" :id "bo")]}]
                            :player-id-in-turn "p1")
                    (attack "p1" "bk" "bo")
                    (get-health "bo"))
                3)
           (is= (-> (create-game [{:minions [(create-minion "The Black Knight" :id "bk")]}
                                  {:hero "Anduin Wrynn"
                                   :minions [(create-minion "Boulderfist Ogre" :id "bo")]}]
                                 :player-id-in-turn "p1")
                    (attack "p1" "bk" "h2")
                    (get-health "h2"))
                26))}
  [state player-id attacker-id target-id]
  (let [target-type (-> (get-character state target-id) (:entity-type))
        target-player-id (get-owner state target-id)]
    (as-> (update state :action-index inc) $
          (if (= target-type :hero)
            (minion-attack-hero $ attacker-id target-player-id)
            (minion-attack-minion $ player-id attacker-id target-id))
          (assoc-event $ {:name "after-attack"}))))