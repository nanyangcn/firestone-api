(ns firestone.definitions
  (:require [ysera.test :refer [is is= is-not error?]]
            [ysera.error :refer [error]]))

; Here is where the definitions are stored
(defonce definitions-atom (atom {}))

(defn add-definitions!
  "Adds the given definitions to the game."
  [definitions]
  (swap! definitions-atom merge definitions))

(defn get-definitions
  "Returns all definitions in the game."
  []
  (vals (deref definitions-atom)))

(defn get-definition
  "Gets the definition identified by the name. Note that this is a none pure function. It depends on the definitions-atom."
  {:test (fn []
           (is= (get-definition "Boulderfist Ogre")
                {:name "Boulderfist Ogre"
                 :attack 6
                 :health 7
                 :mana-cost 6
                 :set :basic
                 :type :minion})
           ; The name can be present in a map with :name as a key
           (is= (get-definition {:name "Boulderfist Ogre"})
                (get-definition "Boulderfist Ogre"))

           (error? (get-definition "Something that does not exist")))}
  [name-or-entity]
  {:pre [(or (string? name-or-entity)
             (and (map? name-or-entity)
                  (contains? name-or-entity :name)))]}
  (let [name (if (string? name-or-entity)
               name-or-entity
               (:name name-or-entity))
        definitions (deref definitions-atom)
        definition (get definitions name)]
    (when (nil? definition)
      (error (str "The name " name-or-entity " does not exist. Are the definitions loaded?")))
    definition))
