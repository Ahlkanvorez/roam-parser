(ns roam.core
  (:require [clojure.string :as string]))

(def group-complement
  {"]]" "[["
   "))" "(("
   "}}" "{{"
   "$$" "$$"
   "^^" "^^"
   "**" "**"
   "__" "__"})

(def type-for {"]]" :link
               "))" :ref
               "}}" :roam-render
               "$$" :latex
               "^^" :highlight
               "**" :bold
               "__" :italic})

(defn group-start? [s]
  (some #(string/starts-with? s %)
        (vals group-complement)))

(defn group-end? [s]
  (some #(string/starts-with? s %)
        (keys group-complement)))

(def open-for (partial get group-complement))

(defn aggregate-groups [accum]
  (->> (reduce (fn [accum d]
                 (let [prior (or (last accum) "")]
                   (if (and (string? prior)
                            (string? d))
                     (conj (vec (butlast accum))
                           (str prior d))
                     (conj accum d))))
               []
               accum)
       (mapv (fn [n]
               (if (string? n)
                 {:text (apply str n)}
                 n)))))

(defn parse-group [stack end]
  (let [target (open-for end)]
    (loop [stack (conj stack end)
           accum ()]
      (if-let [c (first stack)]
        (cond
          (and (= c target) (not (empty? (butlast accum))))
          {:node {(type-for end) (aggregate-groups (butlast accum))}
           :unused (rest stack)}
          :default (recur (rest stack) (conj accum c)))
        (loop [accum accum
               nodes []]
          (if (empty? (remove #{target} accum))
            {:node nil
             :unused (apply str accum)}))))))

(defn tokens->tree [tokens]
  (loop [accum ()
         tokens (reverse tokens)]
    (if-let [v (first tokens)]
      (cond
        (group-end? v)
        (let [{:keys [node unused]} (parse-group accum v)]
          (if (nil? node)
            (recur (conj accum v) (rest tokens))
            (if-not (empty? unused)
              (recur (conj unused node) (rest tokens))
              (recur (list node) (rest tokens)))))
        :default (recur (conj accum v)
                        (rest tokens)))
      (if (= (count accum) 1)
        (first accum)
        (let [aggregate (aggregate-groups (reverse accum))]
          (if (= (count aggregate) 1)
            (first aggregate)
            {:tree aggregate}))))))

(defn str->tokens [s]
  (loop [s s
         stack ()]
    (if (empty? s)
      stack
      (if (or (group-start? s) (group-end? s))
        (recur (subs s 2)
               (conj stack (subs s 0 2)))
        (recur (subs s 1)
               (conj stack (subs s 0 1)))))))

(defn str->tree [s]
  (-> s str->tokens tokens->tree))

(defmulti tree->str (comp first keys))

(defmethod tree->str :text [tree] (:text tree))
(defmethod tree->str :link [tree]
  (str "[["
       (apply str (map tree->str (:link tree)))
       "]]"))
(defmethod tree->str :tree [tree]
  (apply str (map tree->str (:tree tree))))

(def parse str->tree)
