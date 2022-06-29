(ns firestone.client.kth.adapter
  (:require [ysera.test :refer [is is= is-not]]
            [clojure.spec.alpha :as spec]
            [firestone.construct :refer :all]
            [firestone.core :refer :all]
            [firestone.definitions :refer [get-definition]]
            [firestone.client.kth.spec]))

(defn to-client-hero-power
  {:test (fn []
           (let [client-hero-power (let [state (create-game)]
                                 (->> (get-heroes state)
                                      (first)
                                      (to-client-hero-power state)))]
             (is (or (spec/valid? :firestone.client.kth.spec/hero-power client-hero-power)
                     (spec/explain :firestone.client.kth.spec/hero-power client-hero-power)))))}
  [state hero]
  (let [hero-name (:name hero)
        id (:id hero)
        hero-def (get-definition hero-name)
        hero-power-def (get-definition (:hero-power hero))
        hero-power-name (:name hero-power-def)
        owner-id (get-owner state id)
        player-id (:player-id-in-turn state)
        mana            (:mana (get-player state player-id))
        mana-cost       (:mana-cost hero-power-def)]
    {:can-use            (and (:can-use hero-def) (>= mana mana-cost) (hero-power-available? state owner-id) (= owner-id player-id))
     :owner-id           owner-id
     :entity-type        (:entity-type hero-power-def)
     :has-used-your-turn (not (hero-power-available? state owner-id))
     :name               (:name hero-power-def)
     :description        (:description hero-power-def)
     :mana-cost          (or (:mana-cost hero-power-def) 0)
     :original-mana-cost (or (:mana-cost hero-power-def) 0)
     :valid-target-ids   (get-valid-target-ids state player-id hero-power-name)}))

(defn to-client-hero
  {:test (fn []
           (let [client-player (let [state (create-game)]
                                 (->> (get-players state)
                                      (first)
                                      (:hero)
                                      (to-client-hero state)))]
             (is (or (spec/valid? :firestone.client.kth.spec/hero client-player)
                     (spec/explain :firestone.client.kth.spec/hero client-player)))))}
  [state hero]
  (let [id (:id hero)
        owner-id (get-owner state id)
        hero-name (:name hero)
        definition (get-definition hero-name)]
    {:armor            (or (:armor hero) 0)
     :owner-id         owner-id
     :entity-type      (name (:entity-type hero))
     :attack           (get-attack state id)
     :can-attack       (can-attack? state id)
     :health           (get-health state id)
     :max-health       (:health definition)
     :id               id
     :mana             (get-mana state owner-id)
     :max-mana         10
     :name             hero-name
     :states           []
     :valid-attack-ids (get-valid-attack-ids state owner-id)
     :hero-power       (to-client-hero-power state hero)}))

(defn to-client-set
  {:test (fn []
           (is=
             (to-client-set "The Grand Tournament")
             "the-grand-tournament"))}
  [set]
  (-> (name set)
      (clojure.string/lower-case)
      (clojure.string/replace #" " "-")))

(defn to-client-board
  {:test (fn []
           (let [client-board (let [state (create-game [{:minions ["Boulderfist Ogre"]
                                                         :deck    ["Injured Blademaster"]
                                                         :hand    ["Silver Hand Recruit"]}
                                                        {:hero "Anduin Wrynn"}]
                                                       :player-id-in-turn "p1")]
                                 (->> (get-minions state "p1")
                                      (first)
                                      (to-client-board state)))]
             (is (or (spec/valid? :firestone.client.kth.spec/minion client-board)
                     (spec/explain :firestone.client.kth.spec/minion client-board)))))}
  [state card]
  (let [id (:id card)
        card-name (:name card)
        definition (get-definition card-name)
        player-id (get-player-id-in-turn state)]
    (merge {:attack            (get-attack state id)
            :can-attack        (can-attack? state id)
            :entity-type       (name (:entity-type card))
            :health            (get-health state id)
            :id                id
            :name              card-name
            :mana-cost         (:mana-cost definition)
            :max-health        (:health definition)
            :original-attack   (:attack definition)
            :original-health   (:health definition)
            :owner-id          (get-owner state id)
            :position          (:position card)
            :set               (to-client-set (:set definition))
            :sleepy            (sleepy? state id)
            :states            (get-states state card)
            :valid-attack-ids  (get-valid-attack-ids state player-id)
            ;:class-group
            :description       (or (:description definition) "")
            ;:display-name
            ;:external-id
            :rarity            (name (or (:rarity definition) "none"))}
           (when-let [class (:class definition)]
             {:class (name class)}))))

(defn to-client-hand
  {:test (fn []
           (let [client-hand (let [state (create-game [{:minions ["Boulderfist Ogre"]
                                                         :deck    ["Injured Blademaster"]
                                                         :hand    ["Silver Hand Recruit"]}
                                                        {:hero "Anduin Wrynn"}]
                                                       :player-id-in-turn "p1")]
                                (->> (get-hand state "p1")
                                     (first)
                                     (to-client-hand state "p1")))]
             (is (or (spec/valid? :firestone.client.kth.spec/card client-hand)
                     (spec/explain :firestone.client.kth.spec/card client-hand)))))}
  [state player-id card]
  (let [id (:id card)
        card-name (:name card)
        definition (get-definition card-name)]
    (merge {:entity-type           (name (:entity-type card))
            :name                  card-name
            :mana-cost             (:mana-cost definition)
            :original-mana-cost    (:mana-cost definition)
            :type                  (name (:type definition))
            :owner-id              (:owner-id card)
            :id                    id
            :attack                (:attack definition)
            :original-attack       (:attack definition)
            :health                (:health definition)
            :original-health       (:health definition)
            ;:class-group
            :armor                 (or (:armor card) 0)
            :playable              true
            :rarity                (name (or (:rarity definition) "none"))
            :description           (or (:description definition) "")
            ;:special-effect-active
            :valid-target-ids      (get-valid-target-ids state player-id card)}
           (when-let [class (:class definition)]
             {:class (name class)}))))

(defn to-client-player
  {:test (fn []
           (let [client-player (let [state (create-game)]
                                 (->> (get-players state)
                                      (first)
                                      (to-client-player state)))]
             (is (or (spec/valid? :firestone.client.kth.spec/player client-player)
                     (spec/explain :firestone.client.kth.spec/player client-player))))
           )}
  [state player]
  (let [id (:id player)
        board (get-minions state id)
        hand (get-hand state id)
        deck (get-deck state id)]
    {:board-entities (map (fn [p]
                            (to-client-board state p)) board)
     :active-secrets []
     :hand           (map (fn [p]
                            (to-client-hand state id p)) hand)
     :deck-size      (count deck)
     :id             id
     :hero           (to-client-hero state (:hero player))}))

(defn to-client-event
  [state player-id]
  (merge {:name (:name (:event state))}
         (when-let [card (:card (:event state))]
           {:card (to-client-hand state player-id card)})))

(defn to-client-state
  {:test (fn []
           (let [client-state (->> (create-game)
                                   (to-client-state))]
             (is (or (spec/valid? :firestone.client.kth.spec/game-states client-state)
                     (spec/explain :firestone.client.kth.spec/game-states client-state))))
           )}
  [state]
  (let [player-id (:player-id-in-turn state)]
    (println (to-client-event state player-id))
    [{:id             "some-id"
      :player-in-turn player-id
      :players        (->> (get-players state)
                           (map (fn [p]
                                  (to-client-player state p))))
      :action-index   (:action-index state)
      :event          (to-client-event state player-id)
      ;:game-blocker
      :supports-redo  true
      :supports-undo  true
      }]))

