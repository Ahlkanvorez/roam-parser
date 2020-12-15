(ns roam.core-test
  (:require [clojure.test :refer :all]
            [roam.core :as core]))

(deftest hello
  (is (= (name :a) "a"))
  (is (= (name :b) "b")))

(deftest cheese
  (is (> (count "cheddar") (count "brie"))))
