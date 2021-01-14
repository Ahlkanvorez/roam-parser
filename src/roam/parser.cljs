(ns roam.parser
  (:require [clojure.string :as string]
            [clojure.set]
            [roam.parser.roam :as roam]))

(defn parse [s] (roam/parse s))
(def tree->str roam/tree->str)
