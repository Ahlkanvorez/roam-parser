(ns roam.parser.syntax
  (:require [clojure.string :as string]
            [roam.parser.tree :as tree]
            [roam.parser.lexical :as lexical :refer [node->str]]))

(def higher-rules
  [(lexical/Rule. :link      {:bracket :bracket})
   (lexical/Rule. :ref       {:paren :paren})
   (lexical/Rule. :render    {:code :code})
   (lexical/Rule. :highlight {:carrot :carrot-body})
   (lexical/Rule. :bold      {:star :star-body})
   (lexical/Rule. :italic    {:underline :underline-body})
   (lexical/Rule. :latex     {:dollar :dollar-body})
   (lexical/Rule. :syntax    {:syntax :syntax-body})
   (lexical/Rule. :quote     {:quote :quote-body})
   (lexical/Rule. :block     [:syntax-e :syntax :syntax-e])
   (lexical/Rule. :alias     [:bracket :paren])])

(defmethod node->str :alias [n]
  (let [[lhs rhs] (.-children n)]
    (str (node->str (tree/Node. :bracket lhs))
         (node->str (tree/Node. :paren rhs)))))

(defmethod node->str :block [n]
  (str (node->str (tree/Node. :syntax-e []))
       (node->str (tree/Node. :syntax (.-children n)))
       (node->str (tree/Node. :syntax-e []))))

(def rule-ends
  (apply
   merge
   (transduce
    (comp
     (map
      (fn [rule]
        (let [p (.-pattern rule)]
          {(.-name rule)
           (when (map? p)
             (if (string/ends-with? "-body" (name (first (vals p))))
               (get lexical/rule-ends (first (first p)))
               (let [[oo oc] (get lexical/rule-ends (first (keys p)))
                     [io ic] (get lexical/rule-ends (first (vals p)))]
                 [(str oo io) (str ic oc)])))})))
     (remove (comp nil? first vals)))
    conj
    higher-rules)))

(defn nested-node->str [n]
  (let [kind (.-kind n)
        [open close] (get rule-ends kind)]
    (str open (apply str (map lexical/node->str (.-children n))) close)))

(defmethod node->str :link [n] (nested-node->str n))
(defmethod node->str :ref [n] (nested-node->str n))
(defmethod node->str :render [n] (nested-node->str n))
(defmethod node->str :highlight [n] (nested-node->str n))
(defmethod node->str :bold [n] (nested-node->str n))
(defmethod node->str :italic [n] (nested-node->str n))
(defmethod node->str :latex [n] (nested-node->str n))
(defmethod node->str :syntax [n] (nested-node->str n))

(defn rules-for [node]
  (filter (fn [rule]
            (let [p (.-pattern rule)]
              (if (map? p)
                (= (.-kind node) (first (first p)))
                (vector? p))))
          higher-rules))

(defn transform-for [rule]
  (let [p (.-pattern rule)
        n (.-name rule)]
    (if (map? p)
      (fn [node]
        (let [[outer inner] (first p)]
          (if (and (= outer (.-kind node))
                   (= inner (.-kind (first (.-children node))))
                   (= 1 (count (.-children node))))
            (tree/Node. n (-> node .-children first .-children))
            node)))
      (fn [node]
        (if (= :text (.-kind node))
          node
          (let [c (count p)]
            (loop [kids (.-children node)
                   seen []
                   updated false]
              (if (seq kids)
                (if (= (mapv #(.-kind %) (take c kids)) p)
                  (let [grandkids (->> (take c kids)
                                       (map #(.-children %))
                                       (remove empty?)
                                       vec)
                        child (tree/Node. n (if (= 1 (count grandkids))
                                                 (first grandkids)
                                                 grandkids))]
                    (recur (drop c kids)
                           (conj seen child)
                           true))
                  (recur (rest kids) (conj seen (first kids)) updated))
                (if updated
                  (tree/Node. (.-kind node) (into seen (drop c kids)))
                  node)))))))))

(def transforms
  (apply merge
         (map (fn [rule] {(.-name rule) (transform-for rule)})
              higher-rules)))

(defn analyze-node [node]
  (if (string? node)
    node
    (loop [transforms (map #(get transforms (.-name %))
                           (rules-for node))]
      (if-let [transform (first transforms)]
        (let [new-node (transform node)]
          (if (= node new-node)
            (recur (rest transforms))
            new-node))
        node))))

(defn analyze [parse-tree]
  (let [res (tree/map-tree #(analyze-node %2) parse-tree)]
    (if (and (= :tree (.-kind res))
             (= 1 (count (.-children res))))
      (first (.-children res))
      res)))
