(ns firestone.client.api
  (:require [firestone.construct :refer [create-game]]
            [firestone.core-api :refer [end-turn
                                        play-minion-card
                                        attack
                                        do-hero-power
                                        do-spell]]
            [firestone.definitions-loader]))

(def state-atom (atom {:states ()
                       :index 0}))

(defn create-game!
  []
  (-> (reset! state-atom {:states [(create-game [{:minions []
                                    :deck    [
                                              "Mukla's Champion"
                                              "Knife Juggler"
                                              ]
                                    :hand    [
                                              ]}
                                   {:hero "Anduin Wrynn"
                                    :minions []
                                    :deck    [
                                              "Frothing Berserker"
                                              "Hogger"
                                              ]
                                    :hand    [
                                              ]}]
                                  :player-id-in-turn "p1")]
                          :index  0})
      (:states)
      (last)))

(defn get-state!
  []
  (let [state-atom (deref state-atom)
        index (:index state-atom)
        states (:states state-atom)]
   (-> (filter
      (fn [p] (= (:action-index p) index))
      states)
       (first))))

(defn add-state!
  [new-state]
  (let [state-atom-now (deref state-atom)
        index (:index state-atom-now)
        states (:states state-atom-now)
        new-states (-> (remove
                         (fn [p] (> (:action-index p) index))
                         states)
                       (conj new-state))]
    (-> (reset! state-atom {:states new-states
                            :index (inc index)})
        (:states)
        (last))))

(defn end-turn!
  [player-id]
  (let [state (get-state!)]
    (->> (end-turn state player-id)
         (add-state!))
    (get-state!)))

(defn play-minion-card!
  [player-id card-id position target-id]
  (let [state (get-state!)
        new-state (play-minion-card state player-id card-id position :target-id target-id)]
    (when (not (= state new-state))
      (add-state! new-state)))
  (get-state!))

(defn play-spell-card!
  [player-id card-id target-id]
  (let [state (get-state!)]
    (if-let [target-id target-id]
      (let [new-state (do-spell state card-id :target-id target-id)]
        (when (not (= state new-state))
          (add-state! new-state)))
      (let [new-state (do-spell state card-id)]
        (when (not (= state new-state))
          (add-state! new-state)))))
  (get-state!))

(defn attack!
  [player-id attacker-id target-id]
  (let [state (get-state!)]
    (->> (attack state player-id attacker-id target-id)
         (add-state!)))
  (get-state!))

(defn use-hero-power!
  [player-id target-id]
  (let [state (get-state!)]
    (if-let [target-id target-id]
      (let [new-state (do-hero-power state player-id :target-id target-id)]
        (when (not (= state new-state))
          (add-state! new-state)))
      (let [new-state (do-hero-power state player-id)]
        (when (not (= state new-state))
          (add-state! new-state)))))
  (get-state!))

(defn redo!
  []
  (swap! state-atom update :index inc)
  (get-state!))

(defn undo!
  []
  (swap! state-atom update :index dec)
  (get-state!))

(comment
  (create-game!)
  @state-atom
  )