(ns firestone.all
  (:require [clojure.test :refer [run-tests successful?]]
            [ysera.test :refer [deftest is]]
            [firestone.construct]
            [firestone.core]
            [firestone.core-api]
            [firestone.definitions]
            [firestone.definitions-loader]))

(deftest test-all
         "Bootstrapping with the required namespaces, finds all the firestone.* namespaces (except this one),
         requires them, and runs all their tests."
         (let [namespaces (->> (all-ns)
                               (map str)
                               (filter (fn [x] (re-matches #"firestone\..*" x)))
                               (remove (fn [x] (= "firestone.all" x)))
                               (map symbol))]
           (is (successful? (time (apply run-tests namespaces))))))
