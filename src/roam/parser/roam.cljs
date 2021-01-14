(ns roam.parser.roam
  (:require [clojure.walk :as walk]
            [roam.parser.peg :as peg]))

(def type->open-close
  {:link ["[[" "]]"]
   :ref ["((" "))"]
   :paren ["(" ")"]
   :bracket ["[" "]"]
   :render ["{{" "}}"]
   :code ["{" "}"]
   :highlight ["^^" "^^"]
   :bold ["**" "**"]
   :italic ["__" "__"]
   :latex ["$$" "$$"]
   :quote ["\"" "\""]
   :block ["```" "```"]
   :syntax ["`" "`"]})

(def type->open-mid-close
  {:alias ["[" "](" ")"]})

(def terminal-char
  #{"\\" "{" "}" "[" "]" "(" ")" "^" "*" "_" "$" "\"" "`"})

(defn character [c] (not (terminal-char c)))

(defn open-close-rule-for [kind]
  (let [[open close] (type->open-close kind)]
    {kind {[ open {:tree '+} close ] '.}}))

(defn open-mid-close-rule-for [kind]
  (let [[open mid close] (type->open-mid-close kind)]
    {kind {[ open {:tree '+} mid {:tree '+} close ] '.}}))

(def uncompiled-rules
  (merge {:text   {{[character :escape] '|} '+}
          :escape {["\\" terminal-char] '.}
          :tree   {{[:text :escape :alias :link :ref :render :code
                     :highlight :bold :italic :latex :quote :block
                     :syntax :paren :bracket] '|}
                   '+}}
         (->> type->open-mid-close keys (map open-mid-close-rule-for)
              (apply merge))
         (->> type->open-close keys (map open-close-rule-for)
              (apply merge))))

(def rules (peg/compile-all uncompiled-rules))

(def root-rule (:tree rules))

(defn collapse-tree-nodes [n]
  (if (map? n)
    (let [[k v] (first n)
          c (first v)]
      (cond (and (= :tree k) (= 1 (count v))) c
            (= :alias k) {k [(collapse-tree-nodes (first c))
                             (collapse-tree-nodes (first (second v)))]}
            (and (keyword? k) (= 1 (count v)) (vector? c))
            {k (doall (mapv collapse-tree-nodes c))}
            :else n))
    n))

(defn join-strings [n]
  (if (and (vector? n) (= :text (first n)))
    [:text (reduce (fn [accum s]
                     (if (string? s)
                       (if-let [n (last accum)]
                         (if (string? n)
                           (conj (vec (butlast accum)) (str n s))
                           (conj accum s))
                         (conj accum s))
                       (conj accum s)))
                   []
                   (second n))]
    n))

(defn simplify [tree]
  (let [res (walk/prewalk (comp join-strings collapse-tree-nodes) tree)]
    (if (= 1 (count res))
      (first res)
      {:tree res})))

(defn parse [s]
  (if-let [^Result res (root-rule s)]
    (if (pos? (count (.-s res)))
      {:text [(apply str s)]}
      (simplify (.-v res)))
    {:text [(apply str s)]}))

(defmulti tree->str (fn [t]
                      (if (string? t)
                        :string
                        (first (first t)))))

(defn- kids->str [n]
  (apply str (map tree->str (second (first n)))))

(defn- node->str [open n close]
  (str open (apply str n) close))

(defn- one-child->str [open n close]
  (node->str open (map tree->str (second (first n))) close))

(defmethod tree->str :tree [t] (kids->str t))
(defmethod tree->str :string [t] t)
(defmethod tree->str :escape [t] (str "\\" (tree->str (second (first t)))))
(defmethod tree->str :text [t] (kids->str t))

(defmethod tree->str :default [t]
  (let [kind (first (first t))]
    (if-let [[open close] (type->open-close kind)]
      (one-child->str open t close)
      (if-let [[open mid close] (type->open-mid-close kind)]
        (let [sides (second (first t))]
          (str open (tree->str (first sides))
               mid (tree->str (second sides))
               close))))))
