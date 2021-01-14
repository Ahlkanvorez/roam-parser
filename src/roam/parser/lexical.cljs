(ns roam.parser.lexical
  (:require [clojure.string :as string]
            [roam.parser.tree :as tree]))

(defrecord Rule [name pattern])

(defn body-rule-for [rules rule-name]
  (let [name-e (keyword (str (name rule-name) "-e"))
        name-body (keyword (str (name rule-name) "-body"))
        rules (remove #(string/starts-with? (name (.-name %))
                                            (name rule-name))
                      rules)]
    (Rule. name-body [(set (map #(.-name %) rules)) '+])))

(def generator-rules
  [(Rule. :escape      '["\\" :character])
   (Rule. :syntax      '["`" :syntax-body "`"])
   (Rule. :code        '["{" :tree + "}"])
   (Rule. :paren       '["(" :tree + ")"])
   (Rule. :bracket     '["[" :tree + "]"])
   (Rule. :carrot      '["^" "^" :carrot-body "^" "^"])
   (Rule. :star        '["*" "*" :star-body "*" "*"])
   (Rule. :underline   '["_" "_" :underline-body "_" "_"])
   (Rule. :dollar      '["$" "$" :dollar-body "$" "$"])
   (Rule. :quote       '["\"" :quote-body "\""])
   (Rule. :text        '[:non-token +])])

(def base-rules
  (let [rules (conj generator-rules (Rule. :syntax-e    '["`" "`"]))]
    (->> [:syntax :carrot :star :underline :dollar :quote]
         (map (partial body-rule-for rules))
         (into rules))))

(def rule-ends
  (->> base-rules
       (map (fn [rule]
              (let [p (.-pattern rule)
                    lhs (take-while string? p)
                    rem (drop-while string? p)
                    rhs (->> (drop-while (complement string?) rem)
                             (take-while string?))]
                {(.-name rule) [(apply str lhs) (apply str rhs)]})))
       (apply merge)))

(defmulti node->str
  (fn [n]
    (if (string? n)
      :string
      (.-kind n))))

(defmethod node->str :string [n] n)

(defmethod node->str :default [n]
  (let [[open close] (get rule-ends (.-kind n))]
    (str open (apply str (map node->str (.-children n))) close)))

(def tokens
  (transduce (comp (mapcat #(.-pattern %))
                   (filter string?))
             conj
             #{}
             base-rules))

(def max-token-size (apply max (map count tokens)))

(defn rules-named [n]
  (filter #(= n (.-name %)) base-rules))

(defn rules-starting-with [token]
  (let [rules (filter #(= token (first (.-pattern %))) base-rules)]
    (if (seq rules)
      rules
      (rules-named :text))))

(defn rules-for [s]
  (if-let [t (first s)]
    (if (contains? tokens t)
      (rules-starting-with t)
      (rules-named :text))))

(declare parse-node)

(defn parse-at-least-one [s do-parse]
  (loop [success? nil
         s s
         results []]
    (if (seq s)
      (if-let [[res rem] (do-parse s)]
        (recur true rem (conj results res))
        (when success? [results s]))
      [results ()])))

(defn parse-non-token+ [s]
  (let [[res rem]
        (loop [accum []
               s s]
          (if (seq s)
            (if (contains? tokens (first s))
              [accum s]
              (recur (conj accum (first s)) (rest s)))
            [accum s]))]
    (when (seq res)
      [(apply str res) rem])))

(defn parse-node+ [rules-fn]
  (fn [s] (parse-node (rules-fn s) s)))

(defn parse-fn-for [parse-type]
  (case parse-type
    :tree (parse-node+ (comp rules-starting-with first))
    :non-token parse-non-token+
    (parse-node+
     (if (set? parse-type)
       (fn [_s] (mapcat rules-named parse-type))
       (fn [_s] (rules-named parse-type))))))

(defn parse+ [s parse-type]
  (parse-at-least-one s (parse-fn-for parse-type)))

(defn try-parse+ [s e pattern]
  (when-let [[nodes rem] (parse+ s e)]
    [:continue (rest (rest pattern)) rem nodes]))

(defn try-parse-alternatives [token s alts pattern]
  (if (contains? alts token)
    [:continue (rest pattern) (rest s) ()]
    (if-let [[n r] (parse-node (mapcat rules-named alts) s)]
      [:continue (rest pattern) r [n]]
      [:ok () s ()])))

(defn try-parse-character [t s pattern]
  (when t
    [:continue (rest pattern) (rest s) [t]]))

(defn try-parse-non-token [t s pattern]
  (when-not (contains? tokens t)
    [:continue (rest pattern) (rest s) [t]]))

(defn try-parse-keyword [s e pattern]
  (when-let [rules (rules-named e)]
    (when-let [[node rem] (parse-node rules s)]
      [:continue (rest pattern) rem [node]])))

(defn try-parse-with [pattern s]
  (let [t (first s)
        e (first pattern)
        o (second pattern)]
    (cond (= o '+) (try-parse+ s e pattern)
          (= t e) [:continue (rest pattern) (rest s) ()]
          (set? e) (try-parse-alternatives t s e pattern)
          (= e :character) (try-parse-character t s pattern)
          (= e :non-token) (try-parse-non-token t s pattern)
          (keyword? e) (try-parse-keyword s e pattern)
          :default [:fail () s])))

(defn parse-with [rule start-s]
  (loop [pattern (seq (.-pattern rule))
         s (seq start-s)
         results []]
    (cond (and (seq s) (seq pattern))
          (when-let [[code pattern rem nodes] (try-parse-with pattern s)]
            (if (= code :continue)
              (recur pattern rem (into results nodes))
              (when (seq results)
                [code results s])))
          (empty? pattern)
          [:ok (tree/Node. (.-name rule) results) s]
          :default [:fail [] s])))

(defn parse-node [rules s]
  (loop [untested-rules rules]
    (when-let [rule (first untested-rules)]
      (if-let [[code node rem] (parse-with rule s)]
        (case code
          :ok [node rem]
          :fail (when-let [untested-rules (next untested-rules)]
                  (recur untested-rules)))
        (recur (rest untested-rules))))))

(defn parse [s]
  (if-let [[node rem] (parse-node [(Rule. :tree '[:tree +])] s)]
    (if (= 1 (count node))
      (get node 0)
      node)
    (if (seq s)
      (tree/Node. :text [(apply str s)])
      (tree/Node. :tree []))))
