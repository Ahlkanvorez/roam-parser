(ns roam.parser.lexical
  (:require [clojure.string :as string]
            [roam.parser.tree :as tree]))

(defrecord Rule [name pattern])

(defn body-rules-for [rules rule-name]
  (let [name-e (keyword (str (name rule-name) "-e"))
        name-body (keyword (str (name rule-name) "-body"))
        rules (remove #(string/starts-with? (name (.-name %)) (name rule-name))
                      rules)]
    (Rule. name-body [(set (map #(.-name %) rules)) '+])))

(def generator-rules
  [(Rule. :syntax      '["`" :syntax-body "`"])
   (Rule. :code        '["{" :tree + "}"])
   (Rule. :paren       '["(" :tree + ")"])
   (Rule. :bracket     '["[" :tree + "]"])
   (Rule. :carrot      '["^" "^" :carrot-body "^" "^"])
   (Rule. :star        '["*" "*" :star-body "*" "*"])
   (Rule. :underline   '["_" "_" :underline-body "_" "_"])
   (Rule. :dollar      '["$" "$" :dollar-body "$" "$"])
   (Rule. :text        '[:non-token +])])

(def base-rules
  (let [rules (conj generator-rules (Rule. :syntax-e    '["`" "`"]))]
    (into rules
          (flatten
           (map (partial body-rules-for rules)
                [:syntax :carrot :star :underline :dollar])))))

(def rule-ends
  (apply merge
         (map (fn [rule]
                (let [p (.-pattern rule)
                      lhs (take-while string? p)
                      rem (drop-while string? p)
                      rhs (->> (drop-while (complement string?) rem)
                               (take-while string?))]
                  {(.-name rule) [(apply str lhs) (apply str rhs)]}))
              base-rules)))

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

(defn tokenize-at [s]
  (loop [size max-token-size]
    (when (pos? size)
      (let [token (apply str (take size s))]
        (if (contains? tokens token)
          token
          (recur (dec size)))))))

(defn tokenize [s]
  (loop [res []
         s (seq s)]
    (if (seq s)
      (if-let [next-token (tokenize-at s)]
        (recur (conj res next-token) (drop (count next-token) s))
        (recur (conj res (first s)) (rest s)))
      res)))

(defn rules-named [n]
  (filter #(= n (.-name %)) base-rules))

(defn rules-starting-with [token]
  (let [rules (filter #(= token (first (.-pattern %))) base-rules)]
    (if (seq rules)
      rules
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

(defn parse+ [s parse-type]
  (let [t (first s)
        parse-fn (case parse-type
                   :tree (parse-node+ #(rules-starting-with (first %)))
                   :non-token parse-non-token+
                   (parse-node+
                    (if (set? parse-type)
                      (fn [_s] (flatten (map #(rules-named %) parse-type)))
                      (fn [_s] (rules-named parse-type)))))]
    (parse-at-least-one s parse-fn)))

(defn parse-with [rule s]
  (loop [desired (seq (.-pattern rule))
         s (seq s)
         results []]
    (cond (and (seq s) (seq desired))
          (let [t (first s)
                e (first desired)
                o (second desired)]
            (cond (= t e) (recur (rest desired) (rest s) results)
                  (= o '+)
                  (when-let [[nodes rem] (parse+ s e)]
                    (recur (rest (rest desired)) rem (into results nodes)))
                  (set? e)
                  (if (contains? e t)
                    (recur (rest desired) (rest s) results)
                    (if-let [[node rem] (parse-node (flatten (map #(rules-named %) e)) s)]
                      (recur (rest desired) rem (conj results node))
                      (when (seq results)
                        [results s])))
                  :default
                  (case e
                    :non-token
                    (when-not (contains? tokens t)
                      (recur (rest desired) (rest s) (conj results t)))
                    :tree
                    (when-let [[node rem] (parse-node (rules-named e) s)]
                      (recur (rest desired) rem (conj results node)))
                    (if (keyword? e)
                      (when-let [rules (rules-named e)]
                        (when-let [[node rem] (parse-node rules s)]
                          (recur (rest desired) rem (conj results node))))))))
          (empty? desired) [(tree/Node. (.-name rule) results) s]
          :default (when (seq results) [results s]))))

(defn parse-node [rules s]
  (loop [rules rules]
    (when-let [rule (first rules)]
      (if-let [[node rem] (parse-with rule s)]
        [node rem]
        (recur (rest rules))))))

(defn rules-for [s]
  (if-let [t (first s)]
    (if (contains? tokens t)
      (rules-starting-with t)
      (rules-named :text))))

(defn parse [s]
  (if-let [[node rem] (parse-with (Rule. :tree '[:tree +]) s)]
    (if (= 1 (count node))
      (get node 0)
      node)
    (tree/Node. :tree [])))
