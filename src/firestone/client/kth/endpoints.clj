(ns firestone.client.kth.endpoints
  (:require [clojure.pprint :refer [pprint]]
            [clojure.data.json :refer [read-str write-str]]
            [firestone.client.kth.api :refer [create-game!
                                              end-turn!
                                              play-minion-card!
                                              play-spell-card!
                                              attack!
                                              use-hero-power!
                                              redo!
                                              undo!]]))

(defn dispatch
  [uri params]
  (println uri)
  (pprint params)
  (cond (= uri "/createGame")
        (create-game!)

        (= uri "/endTurn")
        (let [{player-id :playerId} params]
          (end-turn! player-id))

        (= uri "/playMinionCard")
        (let [{card-id :cardId
               position :position
               target-id :targetId
               player-id :playerId} params]
          (play-minion-card! player-id card-id position target-id))

        (= uri "/playSpellCard")
        (let [{card-id :cardId
               target-id :targetId
               player-id :playerId} params]
          (play-spell-card! player-id card-id target-id))

        (= uri "/attack")
        (let [{attacker-id :attackerId
               target-id :targetId
               player-id :playerId} params]
          (attack! player-id attacker-id target-id))

        (= uri "/useHeroPower")
        (let [{player-id :playerId
               target-id :targetId} params]
          (use-hero-power! player-id target-id))

        (= uri "/redo")
        (redo!)

        (= uri "/undo")
        (undo!)

        :else
        (do (println "Nothing here at:" uri)
            "")
        ))


(defn handle-request! [request]
  (let [uri (:uri request)
        params (read-str (slurp (:body request))
                         :key-fn keyword)
        result (dispatch uri params)]
    {:status  200
     :headers {"Access-Control-Allow-Origin" "https://www.conjoin-it.se"}
     :body    (write-str result)}))

