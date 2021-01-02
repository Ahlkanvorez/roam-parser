(ns roam.parser
  (:require [clojure.string :as string]
            [clojure.set]
            [roam.parser.lexical :as lexical]
            [roam.parser.syntax :as syntax]))

(defn parse [s]
  (-> s lexical/parse syntax/analyze))

(def node->str lexical/node->str)
