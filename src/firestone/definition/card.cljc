(ns firestone.definition.card
  (:require [ysera.test :refer [is is-not is= error?]]
            [firestone.definitions :refer [add-definitions! get-definition get-definitions]]
            [firestone.core :refer :all]
            [firestone.core-api :refer :all]
            [firestone.construct :refer :all]))

(def aura-filters
  {
   "aura-filters"
   {:minions (fn [state {player-id :player-id}] (get-minions state player-id))
    :adjacent (fn [state {player-id :player-id
                          minion-id :minion-id}] (let [minion (get-minion state minion-id)
                                                       minions (get-minions state player-id)
                                                       position (:position minion)]
                                                   (filter (fn [m] (or (= (+ position 1) (:position m))
                                                                       (= (- position 1) (:position m)))) minions)))}
   })

(def card-definitions
  {

   "Acolyte of Agony"
   {:name         "Acolyte of Agony"
    :mana-cost    3
    :attack       3
    :health       3
    :set          :knights-of-the-frozen-throne
    :type         :minion
    :class        :priest
    :rarity       :common
    :lifesteal    true
    :description  "It takes many years of practiced study in order to fully master agony."}

    "Acolyte of Pain"
    {:name         "Acolyte of Pain"
    :mana-cost    3
    :attack       1
    :health       3
    :set          :hall-of-fame
    :type         :minion
    :rarity       :common
    :description  "Whenever this minion takes damage, draw a card."
    :on-damage    (fn [state {player-id :player-id}]
                    (draw-card state player-id))}

   "Ancestral Spirit"
   {:name      "Ancestral Spirit"
    :mana-cost 2
    :set       "Classic"
    :type      :spell
    :class     :shaman
    :description "Give a minion \"Deathrattle: Resummon this minion.\""
    :with-target true
    :spell (fn [state {target-id :target-id
                       player-id :player-id
                       next-position :next-position}]
             (->> (assoc (get-minion state target-id) :deathrattle
                          (fn [state {minion-name :minion-name}]
                                           (add-minion-to-board state player-id
                                                                (create-minion minion-name)
                                                                next-position)))
                  (replace-minion state)))}

   "Archmage Antonidas"
   {:name         "Archmage Antonidas"
    :mana-cost    7
    :attack       5
    :health       7
    :set          :classic
    :type         :minion
    :rarity       :legendary
    :description  "Whenever you cast a spell, put a 'Fireball' spell into your hand."
    :on-spell-cast (fn [state & _]
                     (add-card-to-hand state (:player-id-in-turn state) (create-card "Fireball")))}

   "Argent Watchman"
   {:name      "Argent Watchman"
    :attack    2
    :health    4
    :mana-cost 2
    :set       "The Grand Tournament"
    :type      :minion
    :description "Can't attack. Inspire: Can attack as normal this turn."
    :cannot-attack true
    :inspire (fn [state {minion-id :minion-id}]
               (update-minion state minion-id :cannot-attack false))
    :end-turn (fn [state {minion-id :minion-id}]
                (update-minion state minion-id :cannot-attack true))}

   "Backstab"
   {:name      "Backstab"
    :mana-cost 0
    :set       :basic
    :type      :spell
    :class     :rogue
    :description "Deal 2 damage to an undamaged minion."
    :spell (fn [state {target-id :target-id}]
             (let [target-minion (get-minion state target-id)
                   target-damage (:damage-taken target-minion)]
               (if (= target-damage 0)
                 (attack-minion state target-id 2)
                 state)))}

   "Beneath the Grounds"
   {:name      "Beneath the Grounds"
    :mana-cost 3
    :set       :the-grand-tournament
    :type      :spell
    :class     :rogue
    :rarity    :epic
    :description "Shuffle 3 Ambushes into your opponent's deck. When drawn, you summon a 4/4 Nerubian."
    :spell (fn [state {opponent-id :opponent-id}]
             (let [card (create-card "Nerubian Ambush!")]
               (reduce (fn [state _]
                         (add-card-to-deck state opponent-id card)) state (range 3))))}

   "Boneguard Lieutenant"
   {:name      "Boneguard Lieutenant"
    :attack    3
    :health    2
    :mana-cost 2
    :set       "The Grand Tournament"
    :type      :minion
    :description "Inspire: Gain +1 Health."
    ;TODO: Add health gain attribute
    :inspire (fn [state {minion-id :minion-id}]
               (update-minion state minion-id :health-gain (+ 1)))}

   "Boulderfist Ogre"
   {:name      "Boulderfist Ogre"
    :attack    6
    :health    7
    :mana-cost 6
    :set       :basic
    :type      :minion}

   "Confessor Paletress"
   {:name      "Confessor Paletress"
    :mana-cost 7
    :attack    5
    :health    4
    :set       :the-grand-tournament
    :type      :minion
    :class     :priest
    :rarity    :legendary
    :description "Inspire: Summon a random Legendary minion."
    :inspire   (fn [state {player-id :player-id
                             next-position :next-position}]
                 (let [[state random-legendary-minion] (->> (get-definitions)
                                                            (filter (fn [m] (= (:rarity m) :legendary)))
                                                            (get-random-element state))
                       minion (create-minion (:name random-legendary-minion))]
                   (add-minion-to-board-and-sleep state player-id minion next-position)))
    }

   "Consecration"
   {:name      "Consecration"
    :mana-cost 4
    :set       :basic
    :type      :spell
    :class     :paladin
    :description "Deal 2 damage to all enemies."
    :spell (fn [state {opponent-id :opponent-id}]
             (->> (get-characters state opponent-id)
                  (map :id)
                  (apply attack-characters state 2)))}

   "Convert"
   {:name      "Convert"
    :attack    3
    :mana-cost 2
    :set       "The Grand Tournament"
    :type      :spell
    :description "Put a copy of an enemy minion into your hand."
    :with-target true
    :spell (fn [state {player-id :player-id
                       target-id :target-id}]
             (let [enemy-minion (get-minion state target-id)
                   card-copy (create-card (:name enemy-minion))]
               (add-card-to-hand state player-id card-copy)))}

   "Darkscale Healer"
   {:name      "Darkscale Healer"
    :attack    4
    :health    5
    :mana-cost 5
    :set       :basic
    :type      :minion
    :description "Battlecry: Restore 2 Health to all friendly characters."
    ; NOTE: This should include the hero.
    ; This restoration can not make their health exceed their starting health.
    :battlecry (fn [state {player-id :player-id}]
                 (reduce (fn [state {id :id}]
                           (heal-character state id 2))
                         state (get-characters state player-id)))}

   "Devilsaur"
   {:name      "Devilsaur"
    :attack    5
    :health    5
    :mana-cost 5
    :type      :minion
    :set       :journey-to-un'goro
    :race      :beast
    :rarity    :common}

   "Entomb"
   {:name        "Entomb"
    :mana-cost   6
    :type        :spell
    :class       :priest
    :set         :the-league-of-explorers
    :rarity      :common
    :description "Choose an enemy minion. Shuffle it into your deck."
    :spell     (fn [state {player-id :player-id
                             target-id :target-id}]
                   (->> (get-minion state target-id)
                       (:name)
                       (create-minion)
                       (add-card-to-deck state player-id)))}

   "Dire Wolf Alpha"
   {:name      "Dire Wolf Alpha"
    :mana-cost 2
    :attack    2
    :health    2
    :type      :minion
    :set       :classic
    :race      :beast
    :rarity    :common
    :rush      true
    :description "Adjacent minions have +1 Attack."
    :aura {:filter :adjacent
           :attack-gain 1}}

   "Faceless Manipulator"
   {:name      "Faceless Manipulator"
    :attack    3
    :health    3
    :mana-cost 5
    :set       :classic
    :type      :minion
    :description "Battlecry: Choose a minion and become a copy of it."
    :battlecry (fn [state {player-id :player-id
                           minion-id :minion-id
                           target-id :target-id}]
                 (if (= target-id nil)
                   state
                   (let [minion (get-minion state minion-id)
                         minion-to-copy (get-minion state target-id)]
                     (println minion minion-to-copy)
                     (->> (assoc minion-to-copy :id minion-id :owner-id player-id :position (:position minion))
                          (replace-minion state)))))}

   "Flame Imp"
   {:name        "Flame Imp"
    :attack      3
    :health      2
    :mana-cost   1
    :set         :classic
    :type        :minion
    :description "Battlecry: Deal 3 damage to your hero."
    :battlecry (fn [state {player-id :player-id}]
                 (attack-hero state player-id 3))
    }

   "Fireball"
   {:name        "Fireball"
    :mana-cost   4
    :set         :basic
    :type        :spell
    :description "Deal 6 damage."
    :with-target true
    :spell (fn [state {target-id :target-id}]
             (attack-character state target-id 6))}

   "Floating Watcher"
   {:name        "Floating Watcher"
    :description "Whenever your hero takes damage on your turn, gain +2/+2."
    :attack      4
    :health      4
    :mana-cost   5
    :type        :minion
    :race        :demon
    :set         :goblins-vs-gnomes
    :rarity      :common
    :on-hero-damage (fn [state {minion-id :minion-id
                                player-id :player-id}]
                     (if (= (:player-id-in-turn state) player-id)
                       (-> (add-attack-gain state minion-id 2)
                           (add-health-gain minion-id 2))
                       state))
    }

   "Frothing Berserker"
   {:name      "Frothing Berserker"
    :mana-cost 3
    :attack    2
    :health    4
    :type      :minion
    :class     :warrior
    :set       :classic
    :rarity    :rare
    :description "Whenever a minion takes damage, gain +1 Attack."
    :on-minion-damage (fn [state {minion-id :minion-id}]
                        (add-attack-gain state minion-id 1))
    }

   "Hogger"
   {:name      "Hogger"
    :mana-cost 6
    :attack    4
    :health    4
    :type      :minion
    :set       :classic
    :rarity    :legendary
    :description "At the end of your turn, summon a 2/2 Gnoll with Taunt."
    :end-turn (fn [state {player-id :player-id
                           next-position :next-position}]
                 (add-minion-to-board-and-sleep state player-id (create-minion "Gnoll") next-position))
    }

   "Hogger, Doom of Elwynn"
   {:name      "Hogger, Doom of Elwynn"
    :mana-cost 7
    :attack    6
    :health    6
    :type      :minion
    :set       :whispers-of-the-old-gods
    :rarity    :legendary
    :description "Whenever this minion takes damage, summon a 2/2 Gnoll with Taunt."
    :on-damage (fn [state {player-id :player-id
                           next-position :next-position}]
                 (add-minion-to-board state player-id (create-minion "Gnoll") next-position))
    }

   "Gnoll"
   {:name      "Gnoll"
    :mana-cost 2
    :attack    2
    :health    2
    :type      :minion
    :set       :whispers-of-the-old-gods
    :description "Taunt"
    :taunt     true
    }

   "Houndmaster Shaw"
   {:name      "Houndmaster Shaw"
    :mana-cost 4
    :attack    3
    :health    6
    :type      :minion
    :set       :the-witchwood
    :class     :hunter
    :rarity    :legendary
    :description "Your other minions have Rush."
    :battlecry (fn [state {}]
                 (assoc state :minion-ids-summoned-this-turn []))}

   "Injured Blademaster"
   {:name        "Injured Blademaster"
    :attack      4
    :health      7
    :mana-cost   3
    :set         :classic
    :type        :minion
    :description "Battlecry: Deal 4 damage to HIMSELF."
    :battlecry (fn [state {minion-id :minion-id}]
                 (attack-minion state minion-id 4))}

   "Knife Juggler"
   {:name        "Knife Juggler"
    :description "After you summon a minion, deal 1 damage to a random enemy."
    :cost        2
    :attack      2
    :health      2
    :type        :minion
    :set         :classic
    :rarity      :rare
    :on-summon   (fn [state {opponent-id :opponent-id}]
                   (let [[state random-enemy] (get-random-character state opponent-id)]
                     (if random-enemy
                       (attack-character state (:id random-enemy) 1)
                       state)))}

   "Lightwarden"
   {:name        "Lightwarden"
    :description "Whenever a character is healed. gain +2 Attack."
    :attack      1
    :health      2
    :mana-cost   1
    :type        :minion
    :set         :classic
    :rarity      :rare
    :on-character-heal (fn [state {minion-id :minion-id}]
                (add-attack-gain state minion-id 2))}

   "Light of the Naaru"
   {:name "Light of the Naaru"
    :type :spell
    :class :priest
    :rarity :rare
    :mana-cost 1
    :description "Restore 3 Health. If the target is still damaged, summon a Lightwarden."
    :spell (fn [state {target-id :target-id
                       player-id :player-id
                       next-position :next-position}]
             (as-> (heal-character state target-id 3) $
                   (if (< 0 (get-damage-taken $ target-id))
                     (add-minion-to-board-and-sleep $ player-id (create-minion "Lightwarden") next-position)
                     $)))}

   "Loot Hoarder"
   {:name        "Loot Hoarder"
    :description "Deathrattle: Draw a card."
    :attack      2
    :health      1
    :mana-cost   2
    :type        :minion
    :set         :classic
    :rarity      :common
    :deathrattle (fn [state {player-id :player-id}]
                   (draw-card state player-id))}

   "Militia Commander"
   {:name        "Militia Commander"
    :description "Rush. Battlecry: Gain +3 Attack this turn."
    :attack      2
    :health      5
    :mana-cost   4
    :type        :minion
    :class       :warrior
    :set         :the-witchwood
    :rarity      :rare
    :rush        true
    :battlecry   (fn [state {minion-id :minion-id}]
                   (add-attack-gain state minion-id 3))
    :end-turn    (fn [state {minion-id :minion-id}]
                   (add-attack-gain state minion-id -3))}

   "Mistress of Pain"
   {:name         "Mistress of Pain"
    :mana-cost    2
    :attack       1
    :health       4
    :set          :goblins-vs-gnomes
    :type         :minion
    :subtype      :demon
    :class        :warlock
    :rarity       :common
    :lifesteal    true
    :description  "Her sister is the Mistress of Pane who sells windows and shower doors."}

   "Mukla's Champion"
   {:name        "Mukla's Champion"
    :description "Inspire. Give your other minions +1/+1."
    :attack      4
    :health      3
    :mana-cost   5
    :type        :minion
    :set         :the-grand-tournament
    :race        :beast
    :rarity      :common
    :inspire     (fn [state {minion-id :minion-id
                             player-id :player-id}]
                   (reduce (fn [state minion] (if (not= (:id minion) minion-id)
                                                (-> (add-attack-gain state (:id minion) 1)
                                                    (add-health-gain (:id minion) 1))
                                                state))
                           state (get-minions state player-id)))}

   "Nat, the Darkfisher"
   {:name        "Nat, the Darkfisher"
    :type        :minion
    :class       :rogue
    :set         :whispers-of-the-old-gods
    :mana-cost   2
    :attack      2
    :health      4
    :rarity      :legendary
    :description "At the start of your opponent's turn, they have a 50% chance to draw an extra card."
    :end-turn (fn [state {opponent-id :opponent-id}]
                  (let [[state draw-extra-card?] (->> (list true false)
                                                      (get-random-element state))]
                    (if draw-extra-card?
                      (draw-card state opponent-id)
                      state)))}

   "Nerubian Ambush!"
   {:name        "Nerubian Ambush!"
    :type        :spell
    :class       :rogue
    :set         "The Grand Tourney"
    :mana-cost   3
    :description "Casts When Drawn \n Summon a 4/4 Nerubian for your opponent."
    :cast-when-drawn  (fn [state]
                        (let [player-id (state :player-id-in-turn)
                              player-change-fn {"p1" "p2"
                                                "p2" "p1"}
                              opponent-id (player-change-fn player-id)
                              nerubian (create-minion "Nerubian")
                              num-minions (count (get-in state [:players player-id :minions]))
                              position num-minions]
                          (add-minion-to-board state opponent-id nerubian position)))}

   "Nerubian"
   {:name        "Nerubian"
    :type        :minion
    :class       :rogue
    :set         "The Grand Tournament"
    :mana-cost   4
    :attack      4
    :health      4
    :description ""}


   "Northshire Cleric"
   {:name        "Northshire Cleric"
    :description "Whenever a minion is healed, draw a card."
    :attack      1
    :health      3
    :mana-cost   1
    :type        :minion
    :class       :priest
    :set         :basic
    :on-minion-heal (fn [state {minion-id :minion-id} ]
               (let [minion (get-minion state minion-id)
                     player-id (:owner-id minion)]
                 (draw-card state player-id)))}

   "Princess Huhuran"
   {:name      "Princess Huhuran"
    :attack    6
    :health    5
    :mana-cost 5
    :class     :hunter
    :set       "Whispers of the Old Gods"
    :rarity    :legendary
    :type      :minion
    :description "Battlecry: Trigger a friendly minion's Deathrattle."
    :battlecry (fn [state {target-id :target-id}]
                 (do-deathrattle state target-id))}

   "Ragnaros the Firelord"
   {:name        "Ragnaros the Firelord"
    :attack      8
    :health      8
    :type        :minion
    :set         :hall-of-fame
    :race        :elemental
    :rarity      :legendary
    :description "Can't attack. At the end of your turn, deal 8 damage to a random enemy."
    :cannot-attack true
    :end-turn    (fn [state {opponent-id :opponent-id}]
                   (let [[state character] (get-random-character state opponent-id)]
                     (attack-character state (:id character) 8)))}

   "Ragnaros, Lightlord"
   {:name      "Ragnaros, Lightlord"
    :mana-cost 8
    :attack    8
    :health    8
    :set       :whispers-of-the-old-gods
    :type      :minion
    :subtype   :elemental
    :class     :paladin
    :rarity    :legendary
    :description "At the end of your turn, restore 8 health to a damaged friendly character."
    :end-turn    (fn [state {player-id :player-id}]
                   (let [damaged-characters (->> (get-characters state player-id)
                                                (filter (fn [c] (not= 0 (:damage-taken c)))))
                         [state character] (get-random-element state damaged-characters)]
                     (if character (heal-character state (:id character) 8) state)))
    }

   "Ravaging Ghoul"
   {:name      "Ravaging Ghoul"
    :mana-cost 3
    :attack    3
    :health    3
    :set       :whispers-of-the-old-gods
    :type      :minion
    :class     :warrior
    :rarity    :common
    :description "Battlecry: Deal 1 damage to all other minions."
    :battlecry    (fn [state {minion-id :minion-id}]
                    (reduce (fn [state {id :id}]
                              (if (not= minion-id id)
                                (attack-minion state id 1)
                                state)) state (get-minions state)))
    }

   "Rocket Boots"
   {:name        "Rocket Boots"
    :mana-cost   4
    :set         :basic
    :type        :spell
    :description "Deal 6 damage."
    :with-target true
    :spell (fn [state {target-id :target-id}]
             (attack-character state target-id 6))}

   "Sideshow Spelleater"
   {:name      "Sideshow Spelleater"
    :mana-cost 6
    :attack    6
    :health    5
    :set       :the-grand-tournament
    :type      :minion
    :rarity    :epic
    :description "Battlecry: Copy your opponent's Hero Power."
    :battlecry    (fn [state {player-id :player-id
                              opponent-id :opponent-id}]
                    (->> (get-hero state opponent-id)
                         (:hero-power)
                         (get-definition)
                         (:name)
                         (assoc-in state [:players player-id :hero :hero-power])))
    }

   "Silver Hand Recruit"
   {:name      "Silver Hand Recruit"
    :attack    1
    :health    1
    :mana-cost 1
    :set       :classic
    :type      :minion}

   "Squirrel"
   {:name      "Squirrel"
    :attack    1
    :health    1
    :mana-cost 1
    :set       :classic
    :rarity    :common
    :type      :minion}

   "Tinkmaster Overspark"
   {:name      "Tinkmaster Overspark"
    :mana-cost 3
    :attack    3
    :health    3
    :set       :classic
    :rarity    :legendary
    :type      :minion
    :description "Battlecry: Transform another random minion into a 5/5 Devilsaur or a 1/1 Squirrel."
    :battlecry (fn [state & _]
                 (let [minions (list (create-minion "Devilsaur") (create-minion "Squirrel"))
                       [state minion-to-add] (get-random-element state minions)
                       [state minion-to-remove] (get-random-minion state)]
                   (replace-minion state minion-to-add (:id minion-to-remove))))}

   "Leper Gnome"
   {:name        "Leper Gnome"
    :attack      1
    :health      1
    :mana-cost   1
    :type        :minion
    :set         :classic
    :rarity      :common
    :description "Deathrattle: Deal 2 damage to the enemy hero."
    :deathrattle (fn [state {opponent-id :opponent-id}]
                   (attack-hero state opponent-id 2))}

   "Lowly Squire"
   {:name        "Lowly Squire"
    :attack      1
    :health      2
    :mana-cost   1
    :type        :minion
    :set         :the-grand-tournament
    :rarity      :common
    :description "Inspire: Gain +1 Attack."
    :inspire     (fn [state {minion-id :minion-id}]
                   (add-attack-gain state minion-id 1))}

   "Resurrect"
   {:name        "Resurrect"
    :type        :spell
    :class       :priest
    :set         :blackrock-mountain
    :rarity      :rare
    :description "Summon a random friendly minion that died this game."
    :spell       (fn [state {player-id :player-id
                             next-position :next-position}]
                   (let [[state random-card] (get-random-card-from-graveyard state player-id)
                         healed-minion (assoc random-card :damage-taken 0)]
                     (-> (add-minion-to-board state player-id healed-minion next-position)
                         (remove-minion-from-graveyard player-id (:id random-card)))))}

   "Sprint"
   {:name        "Sprint"
    :type        :spell
    :class       :rogue
    :set         :basic
    :description "Draw 4 cards."
    :spell       (fn [state {player-id :player-id}]
                   (draw-cards state player-id 4))}

   "The Black Knight"
   {:name        "The Black Knight"
    :attack      4
    :health      5
    :mana-cost   6
    :type        :minion
    :set         :classic
    :rarity      :legendary
    :description "Battlecry: Destroy an enemy minion with Taunt."
    :battlecry   (fn [state {opponent-id :opponent-id}]
                   (let [[state random-taunt] (get-random-card-from-taunts state opponent-id)]
                     (if random-taunt
                       (remove-minion state (:id random-taunt))
                       state)))}

   "Unstable Ghoul"
   {:name        "Unstable Ghoul"
    :attack      1
    :health      3
    :type        :minion
    :set         :curse-of-naxxramas
    :rarity      :common
    :taunt       true
    :description "Taunt. Deathrattle: Deal 1 damage to all minions."
    :deathrattle   (fn [state {minion-id :minion-id}]
                     (reduce (fn [state {id :id}] (if (not= minion-id id)
                                                    (attack-minion state id 1)
                                                    state))
                             state (get-minions state)))}

   })

(add-definitions! card-definitions)
(add-definitions! aura-filters)