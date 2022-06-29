(ns firestone.definition.card-test
  (:require [ysera.test :refer :all]
            [firestone.definitions :refer :all]
            [firestone.construct :refer :all]
            [firestone.core :refer :all]
            [firestone.core-api :refer :all]
            [firestone.definition.card :refer :all]))


(deftest acolyte-of-agony
         "It takes many years of practiced study in order to fully master agony."
         (let [state (-> (create-game [{:hero    (create-hero "Uther Lightbringer" :id "h1")
                                        :minions [(create-minion "Acolyte of Agony" :id "aa")]}
                                       {:minions [(create-minion "Boulderfist Ogre" :id "bo")]}])
                         (attack-hero "h1" 3)
                         (minion-attack-minion "p1" "aa" "bo")
                         )]
           (is= (get-health state "h1") 30)))

(deftest acolyte-of-pain
         "Whenever this minion takes damage, draw a card."
         (is= (-> (create-game [{:minions [(create-minion "Acolyte of Pain" :id "ap")]
                                 :deck    ["Injured Blademaster"]}])
              (attack-minion "ap" 1)
              (get-hand "p1")
              (count)) 1))

(deftest argent-watchman
         "Can't attack. Inspire: Can attack as normal this turn."
         (let [minion (create-minion "Argent Watchman" :id "aw")
               state (-> (create-game [(create-hero "Uther Lightbringer" :id "h1")
                                       (create-hero "Anduin Wrynn" :id "h2")])
                         (add-minion-to-board "p1" minion 0)
                         (do-hero-power "p1"))]
           (is= ((get-minion state "aw") :cannot-attack) false)))

(deftest beneath-the-grounds
         "Shuffle 3 Ambushes into your opponent's deck. When drawn, you summon a 4/4 Nerubian."
         (let [state (-> (create-game [{} {:hand [(create-card "Beneath the Grounds" :id "btg")]}])
                         (do-spell "btg")
                         (draw-card "p1"))]
           (-> (get-deck state "p1") (first) (:name)
               (is= "Nerubian Ambush!"))
           (-> (get-minions state "p2") (first) (:name)
               (is= "Nerubian")))
         (let [state (-> (create-game [{:hand [(create-card "Beneath the Grounds" :id "btg")]}])
                         (do-spell "btg")
                         (end-turn "p1"))]
           (-> (get-deck state "p2") (first) (:name)
               (is= "Nerubian Ambush!"))
           (-> (get-minions state "p1") (first) (:name)
               (is= "Nerubian"))))

(deftest boneguard-lieutenant
         "Inspire: Gain +1 Health."
         (let [minion (create-minion "Boneguard Lieutenant" :id "ls")
               start-health (-> (get-definition minion) (:health))
               state (-> (create-game)
                         (add-minion-to-board "p1" minion 0)
                         (do-inspire "ls"))]
           (is= (get-health state "ls")  (+ 1 start-health))))

(deftest confessor-paletress
         "Inspire: Summon a random Legendary minion."
         (is= (-> (create-game [{:minions [(create-minion "Confessor Paletress" :id "cp")]}])
             (do-inspire "cp")
             (get-minions "p1")
             (last)
             (:name)
             (get-definition )
             (:rarity)) :legendary))

(deftest consecration
         "Deal 2 damage to all enemies."
         (is= (as-> (create-game [{:hand [(create-card "Consecration" :id "cc")]}
                                  {:minions [(create-minion "Boulderfist Ogre" :id "bl")]}]) $
                    (do-spell $ "cc")
                    (get-characters $ "p2")
                    (map :damage-taken $)) (list 2 2)))

(deftest convert
           "Put a copy of an enemy minion into your hand."
         (let [state (-> (create-game [{:hand [(create-card "Convert" :id "cv")]}
                                       {:minions [(create-minion "Boneguard Lieutenant" :id "bl")]}])
                         (do-spell "cv" :target-id "bl"))]
           (is= (-> (get-player state "p1")
                    (:hand)
                    (last)
                    (:name)) "Boneguard Lieutenant")))

(deftest darkscale-healer
         "Battlecry: Restore 2 Health to all friendly characters."
         (let [minion (create-minion "Darkscale Healer" :id "dh")
               start-health (-> (get-definition minion) (:health))
               state (-> (create-game [(create-hero "Uther Lightbringer" :id "h1")
                                       (create-hero "Anduin Wrynn" :id "h2")])
                         (add-minion-to-board "p1" minion 0)
                         (update-minion "dh" :damage-taken 2)
                         (attack-hero "p1" 2)
                         (do-battlecry "dh"))]
           (is= (get-health state "dh")  start-health)
           (is= (get-health state "h1")  30)))

(deftest faceless-manipulator
         "Battlecry: Choose a minion and become a copy of it."
         (let [minion (create-minion "Faceless Manipulator" :id "fm")
               minion-to-copy (create-minion "Darkscale Healer" :id "dh")
               state (-> (create-game)
                         (add-minion-to-board "p1" minion 0)
                         (add-minion-to-board "p2" minion-to-copy 0)
                         (do-battlecry "fm" :target-id "dh"))]
           (is= (-> (get-minions state "p1") (first) (:name))  "Darkscale Healer")))

(deftest floating-watcher
         "Whenever your hero takes damage on your turn, gain +2/+2."
         (is= (-> (create-game [{:minions [(create-minion "Floating Watcher" :id "fw")]}])
                  (attack-hero "p1" 3)
                  (get-attack "fw")) (+ 4 2))
         (is= (-> (create-game [{:minions [(create-minion "Floating Watcher" :id "fw")]}])
                  (attack-hero "p1" 3)
                  (get-health "fw")) (+ 4 2))
         (is= (-> (create-game [{:minions [(create-minion "Floating Watcher" :id "fw")]}])
                  (end-turn "p1")
                  (attack-hero "p2" 3)
                  (get-health "fw")) 4))

(deftest fireball
         "Deal 6 damage."
         (let [spell-card (create-card "Fireball" :id "fb")
               enemy-minion-id "rtf"
               enemy-minion (create-minion "Ragnaros the Firelord" :id enemy-minion-id)
               state (-> (create-game [(create-hero "Uther Lightbringer" :id "h1")
                                       (create-hero "Anduin Wrynn" :id "h2")])
                         (add-card-to-hand "p1" spell-card)
                         (add-minion-to-board "p2" enemy-minion 0)
                         (do-spell "fb" :target-id enemy-minion-id))]
           (is= (-> (get-minion state enemy-minion-id) (:damage-taken)) 6)))

(deftest frothing-berserker
         "Whenever a minion takes damage, gain +1 Attack."
         (is= (-> (create-game [{:minions [(create-minion "Frothing Berserker" :id "fb")
                                           (create-minion "Boulderfist Ogre" :id "bo")]}])
                  (attack-minion "bo" 2)
                  (get-minion "fb")
                  (:attack-gain)) 1))

(deftest hogger
         "At the end of your turn, summon a 2/2 Gnoll with Taunt."
         (is= (as-> (create-game [{:minions [(create-minion "Hogger" :id "h")]}]) $
                    (end-turn $ "p1")
                    (get-minions $ "p1")
                    (map :name $)) (list "Hogger" "Gnoll")))

(deftest hogger-doom-of-elwynn
         "Whenever this minion takes damage, summon a 2/2 Gnoll with Taunt."
         (is= (as-> (create-game [{:minions [(create-minion "Hogger, Doom of Elwynn" :id "hd")]}]) $
                  (attack-minion $ "hd" 2)
                  (get-minions $ "p1")
                  (map :name $)) (list "Hogger, Doom of Elwynn" "Gnoll")))

(deftest knife-juggler
         "After you summon a minion, deal 1 damage to a random enemy."
         (let [state (-> (create-game [{:minions [(create-minion "Knife Juggler" :id "kj")
                                      (create-minion "Leper Gnome" :id "lg")]}
                           {:minions [(create-minion "Boulderfist Ogre" :id "bo")]}]))]
           (is (or (= (get-damage-taken state "bo") 1)
                   (= (get-damage-taken state "h2") 1)))))

(deftest leper-gnome
         "Deathrattle: Deal 2 damage to the enemy hero."
         (let [minion (create-minion "Leper Gnome" :id "ib")
               state (-> (create-game [(create-hero "Uther Lightbringer" :id "h1")
                                       (create-hero "Anduin Wrynn" :id "h2")])
                         (add-minion-to-board "p1" minion 0)
                         (do-deathrattle "ib"))]
           (is= (get-health state "h2") 28)))

(deftest lightwarden
         "Whenever a character is healed. gain +2 Attack."
         (is= (-> (create-game [{:minions [(create-minion "Lightwarden" :id "lw")
                                      (create-minion "Boulderfist Ogre" :id "bo")]}])
             (attack-minion "bo" 3)
             (heal-character "bo" 2)
             (get-attack "lw")) (+ 1 2)))

(deftest light-of-the-naaru
         "Restore 3 Health. If the target is still damaged, summon a Lightwarden."
         (let [state (-> (create-game [{:hand [(create-card "Light of the Naaru" :id "ln")]
                                        :minions [(create-minion "Boulderfist Ogre" :id "bo")]}])
                         (attack-minion "bo" 4)
                         (do-spell "ln" :target-id "bo"))]
           (is= (-> (get-minions state "p1") (count)) 2)
           (is (->> (get-minions state "p1")
                    (filter (fn [m] (= "Lightwarden" (:name m))))
                    (first)))
           (is= (-> (get-damage-taken state "bo")) 1)))

(deftest lowly-squire
         "Inspire: Gain +1 Attack."
         (let [minion (create-minion "Lowly Squire" :id "ls")
               state (-> (create-game)
                         (add-minion-to-board "p1" minion 0)
                         (do-inspire "ls"))]
           (is= (get-attack state "ls") 2)))

(deftest militia-commander
         "Rush. Battlecry: Gain +3 Attack this turn."
         (let [state (-> (create-game [{:minions [(create-minion "Militia Commander" :id "mc")]}])
                         (do-battlecry "mc"))]
           (is= (get-attack state "mc") 5)
           (is= (-> (do-end-turn state "mc")
                    (get-attack "mc")) 2)))

(deftest mistress-of-pain
         "It takes many years of practiced study in order to fully master agony."
         (let [state (-> (create-game [{:hero    (create-hero "Uther Lightbringer" :id "h1")
                                        :minions [(create-minion "Mistress of Pain" :id "mp")]}
                                       {:minions [(create-minion "Boulderfist Ogre" :id "bo")]}])
                         (attack-hero "h1" 1)
                         (minion-attack-minion "p1" "mp" "bo")
                         )]
           (is= (get-health state "h1") 30)))

(deftest nat-the-darkfisher
         "At the start of your opponent's turn, they have a 50% chance to draw an extra card."
         (is (-> (create-game [{:minions [(create-minion "Nat, the Darkfisher" :id "nd")]}
                           {:deck [(create-card "Boulderfist Ogre" :id "bo")]}])
             (do-end-turn "nd")
             (get-player "p2")
             (:hand)
             (first)))
         ; This time, set the random seed to make it NOT draw an extra card.
         (is-not (-> (create-game [{:minions [(create-minion "Nat, the Darkfisher" :id "nd")]}
                               {:deck [(create-card "Boulderfist Ogre" :id "bo")]}])
                 (assoc :seed 345)
                 (do-end-turn "nd")
                 (get-player "p2")
                 (:hand)
                 (first))))

(deftest nerubian-ambush
         "Casts When Drawn \n Summon a 4/4 Nerubian for your opponent."
         (let [state (-> (create-game [{:deck [(create-card "Nerubian Ambush!")]}])
                         (draw-card "p1"))]
           (is= (get-hand state "p1") [])
           (is= (-> (get-minions state "p2") (first) (:name)) "Nerubian"))
         (let [state (-> (create-game [{}
                                       {:deck [(create-card "Nerubian Ambush!")]}])
                         (end-turn "p1"))]
           (is= (get-hand state "p2") [])
           (is= (-> (get-minions state "p1") (first) (:name)) "Nerubian")))

(deftest northshire-clerk
         "Whenever a minion is healed, draw a card."
         (is= (-> (create-game [{:minions [(create-minion "Northshire Cleric" :id "nc")
                                           (create-minion "Boulderfist Ogre" :id "bo")]
                                 :deck [(create-card "Boulderfist Ogre")]}])
                  (attack-minion "bo" 3)
                  (heal-character "bo" 2)
                  (get-hand "p1")
                  (count)) 1))

(deftest muklas-champion
         "Inspire. Give your other minions +1/+1."
         (let [state (-> (create-game [{:minions [(create-minion "Mukla's Champion" :id "mc")
                                                  (create-minion "Boulderfist Ogre" :id "bo")
                                                  (create-minion "Northshire Cleric" :id "nc")]}])
                         (do-inspire "mc"))]
           (is= (get-attack state "mc") 4)
           (is= (get-health state "mc") 3)
           (is= (get-attack state "bo") 7)
           (is= (get-health state "bo") 8)
           (is= (get-attack state "nc") 2)
           (is= (get-health state "nc") 4)))

(deftest princess-huhuran
         "Battlecry: Trigger a friendly minion's Deathrattle."
         (is= (-> (create-game [{:minions [(create-minion "Princess Huhuran" :id "ph")
                                           (create-minion "Unstable Ghoul" :id "ug")]}])
              (do-battlecry "ph" :target-id "ug")
              ; Because of the ug's deathrattle, the ph should take 1 damage.
              (get-damage-taken "ph")) 1))

(deftest ragnaros-the-firelord
         "Can't attack. At the end of your turn, deal 8 damage to a random enemy."
         (let [minion (create-minion "Ragnaros the Firelord" :id "rf")
               state (-> (create-game [{:hero (create-hero "Uther Lightbringer" :id "h1")}
                                       {:hero (create-hero "Anduin Wrynn" :id "h2")
                                        :minions [(create-minion "Boulderfist Ogre" :id "bo")
                                                  (create-minion "Unstable Ghoul" :id "ug")]}])
                         (add-minion-to-board "p1" minion 0)
                         (do-end-turn "rf"))]
           (and (is-not (can-attack? state "rf"))
                ; Either one of the minion dies or the hero takes damage
                (is= (or
                       (-> (get-graveyard state "p2") (first) (some?))
                       (= (get-damage-taken state "h2") 8))
                     true))))

(deftest ragnaros-lightlord
         "At the end of your turn, restore 8 health to a damaged friendly character."
         (let [state (-> (create-game [{:minions [(create-minion "Ragnaros, Lightlord" :id "rl")
                                                  (create-minion "Boulderfist Ogre" :id "bo")]}])
                         (attack-character "h1" 5)
                         (attack-character "bo" 5)
                         (do-end-turn "rl"))]
           (is (or
                  (= (get-health state "h1") 30)
                  (= (get-health state "bo") 7)))))

(deftest ravaging-ghoul
         "Battlecry: Deal 1 damage to all other minions."
         (let [state (-> (create-game [{:minions [(create-minion "Ravaging Ghoul" :id "rg")
                                                  (create-minion "Boulderfist Ogre" :id "bo")]}
                                       {:minions [(create-minion "Unstable Ghoul" :id "ug")]}])
                         (do-battlecry "rg"))]
           (is= (get-damage-taken state "rg") 0)
           (is= (get-damage-taken state "bo") 1)
           (is= (get-damage-taken state "ug") 1)
           (is= (get-damage-taken state "h1") 0)))

(deftest rocket-boots
         "Deal 6 damage."
         (let [state (-> (create-game [{:hand [(create-card "Rocket Boots" :id "rb")]}
                                       {:minions [(create-minion "Boulderfist Ogre" :id "bo")]}])
                         (do-spell "rb" :target-id "bo"))]
           (is= (-> (get-minion state "bo") (:damage-taken)) 6)))

(deftest sideshow-spelleater
         "Battlecry: Copy your opponent's Hero Power."
         (let [state (-> (create-game [{:minions [(create-minion "Sideshow Spelleater" :id "sp")]}])
                         (do-battlecry "sp"))]
           (is= (:hero-power (get-hero state "p1"))
                (:hero-power (get-hero state "p2")))))

(deftest sprint
         "Draw 4 cards."
         (let [card (create-card "Sprint" :id "sp")
               state (-> (create-game [{:deck ["Boulderfist Ogre", "Unstable Ghoul"]}])
                         (add-card-to-hand "p1" card)
                         (do-spell "sp"))]
           (and
             (is= (count (get-hand state "p1")) 2)
             (is= (get-damage-taken state "h1") 3)
             (is= (get-fatigue-counter state "p1") 2))))

(deftest resurrect
         "Summon a random friendly minion that died this game."
         (let [card (create-card "Resurrect" :id "rs")
               state (-> (create-game [{:minions [(create-minion "Boulderfist Ogre" :id "bo")
                                                  (create-minion "Unstable Ghoul" :id "ug")]}])
                         (add-card-to-hand "p1" card)
                         (remove-minions "bo" "ug")
                         (do-spell "rs"))]
           (and
             (is= (count (get-minions state "p1")) 1)
             (is= (count (get-graveyard state "p1")) 1))
           ))

(deftest the-black-knight
         "Battlecry: Destroy an enemy minion with Taunt."
         (let [minion (create-minion "The Black Knight" :id "bk")
               taunt (create-minion "Unstable Ghoul" :id "ug")
               state (-> (create-game)
                         (add-minion-to-board "p1" minion 0)
                         (add-minion-to-board "p2" taunt 0)
                         (do-battlecry "bk"))
               minions (get-minions state "p2")
               taunts (get-taunts state "p2")]
           (and (is= minions [])
                (is= taunts []))))

(deftest tinkmaster-overspark
         "Battlecry: Transform another random minion into a 5/5 Devilsaur or a 1/1 Squirrel."
         (let [state (-> (create-game [{:minions [(create-minion "Tinkmaster Overspark" :id "to")
                                                  (create-minion "Boulderfist Ogre" :id "bo")]}
                                       {:minions [(create-minion "Unstable Ghoul" :id "ug")]}])
                         (do-battlecry "to"))
               minions (get-minions state)]
           (is= (count minions) 3)
           (is (as-> (map :name minions) $
                     (filter (fn [name] (or (= name "Devilsaur")
                                            (= name "Squirrel"))) $)
                     (first $)))))

(deftest unstable-ghoul
         "Taunt. Deathrattle: Deal 1 damage to all minions."
         (let [state (-> (create-game [{:minions [(create-minion "Unstable Ghoul" :id "ib")
                                                  (create-minion "Boulderfist Ogre" :id "bo")
                                                  (create-minion "Nerubian" :id "ne")]}
                                       {:minions [(create-minion "Boulderfist Ogre" :id "bo2")]}])
                         (attack-minion "bo" 6)
                         (attack-minion "ib" 3)
                         ;(attack-minion "ne" 1)
                         )]
           (is= (get-health state "bo2") 6)
           (is= (get-health state "ne") 3)
           (is= (-> (get-graveyard state "p1")
                    (first)
                    (:name)) "Boulderfist Ogre")))