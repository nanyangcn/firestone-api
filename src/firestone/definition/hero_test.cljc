(ns firestone.definition.hero-test
  (:require [ysera.test :refer :all]
            [firestone.definitions :refer :all]
            [firestone.construct :refer :all]
            [firestone.core :refer :all]
            [firestone.core-api :refer :all]
            [firestone.definition.card :refer :all]))



(deftest anduin-wrynn
         "Restore 2 health."
         (is= (-> (create-game [{:hero "Uther Lightbringer"}
                           {:hero "Anduin Wrynn" :id "h2"}])
             (attack-hero "p2" 3)
             (do-hero-power "p2" :target-id "h2")
             (get-player "p2")
             :hero
             :damage-taken) 1))
