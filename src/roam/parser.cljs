(ns roam.parser
  (:require [clojure.string :as string]
            [clojure.set]
            [roam.parser.roam :as roam]
            [roam.parser.custom-math :as math]))

(defn parse [s]
  (try (-> (roam/parse s) math/evaluate)
       (catch :default e
         (println e)
         (println (.-stack e)))))

(def tree->str roam/tree->str)
