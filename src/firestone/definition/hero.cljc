(ns firestone.definition.hero
  (:require [firestone.definitions :refer [add-definitions! get-definition]]
            [firestone.core :refer [heal-character]]
            [firestone.construct :refer [get-character
                                         get-player
                                         update-minion
                                         add-minion-to-board-and-sleep
                                         create-minion]]
            [ysera.error :refer [error]]))

(def hero-power-definitions
  {
   "Lesser Heal"
   {:name "Lesser Heal"
    :entity-type "hero-power"
    :description "Restore 2 health."
    :mana-cost 2
    :with-target true
    :hero-power-function (fn [state {target-id :target-id}]
                           (let [character (get-character state target-id)
                                 valid-target? (fn [character] (and character
                                                                    (not= 0 (:damage-taken character))))]
                             (if (valid-target? character)
                               (heal-character state target-id 2)
                               state)))}
   "Reinforce"
   {:name "Reinforce"
    :entity-type "hero-power"
    :description "Summon a 1/1 Silver Hand Recruit."
    :mana-cost 2
    :hero-power-function (fn [state {player-id :player-id}]
                           (let [minion (create-minion "Silver Hand Recruit")
                                 num-minions (count (get-in state [:players player-id :minions]))
                                 position (+ num-minions 0)]
                             (if (>= num-minions 7)
                               (assoc-in state [:players player-id :hero :hero-power-available] false)
                               (add-minion-to-board-and-sleep state player-id minion position))))}
   })

(add-definitions! hero-power-definitions)

(def hero-definitions
  {
   "Anduin Wrynn"
   {:name   "Anduin Wrynn"
    :type   :hero
    :health 30
    :can-use true
    :hero-power "Lesser Heal"
    }

   "Uther Lightbringer"
   {:name   "Uther Lightbringer"
    :type   :hero
    :health 30
    :can-use true
    :hero-power "Reinforce"
    }
   })

(add-definitions! hero-definitions)