(ns roam.parser.custom-math
  (:require [clojure.string :as string]
            [clojure.walk :as walk]
            [roam.parser.roam :as roam]
            ["mathjs" :as math]))

(def math-tag "math:")

(defn evaluate-math-nodes [n]
  (if (and (map? n)
           (= :code (first (keys n))))
    (let [s (roam/tree->str n)
          s (string/replace (subs s 1 (dec (count s))) "\\" "")]
      (if (string/starts-with? s math-tag)
        (let [expr (second (string/split s math-tag))]
          (try {:code [{:text [(str (math/evaluate expr))]}]}
               (catch :default e
                 (println e)
                 n)))
        n))
    n))

(defn evaluate [tree]
  (walk/prewalk evaluate-math-nodes tree))
