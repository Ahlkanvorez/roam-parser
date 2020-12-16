(ns roam.core
  (:require [clojure.string :as string]))

(def group-complement
  {"]]" "[["
   "))" "(("
   "}}" "{{"
   "$$" "$$"
   "^^" "^^"
   "**" "**"
   "__" "__"
   ")" "["})

(def group-trios
  {")" {:open "["
        :middle "]("
        :close ")"}})

(defn middle-token-for [trio-close-token]
  (get-in group-trios [trio-close-token :middle]))

(def two-char-tokens
  (set (concat (keys group-complement)
               (vals group-complement)
               (filter #(= 2 (count %)) (map :open (vals group-trios)))
               (filter #(= 2 (count %)) (map :middle (vals group-trios)))
               (filter #(= 2 (count %)) (map :close (vals group-trios))))))

(def open-for (partial get group-complement))

(def type-for {"]]" :link
               "))" :ref
               "}}" :roam-render
               "$$" :latex
               "^^" :highlight
               "**" :bold
               "__" :italic
               ")" :alias})

(def regex-for (partial get {"](" #"\]\("}))

(def close-for-type (apply merge (map (fn [[k v]] {v k}) type-for)))

(defn group-start? [s]
  (some #(string/starts-with? s %)
        (vals group-complement)))

(defn group-end? [s]
  (some #(string/starts-with? s %)
        (keys group-complement)))

(defn concatenate-adjacent-strings [accum d]
  (let [prior (or (last accum) "")]
    (if (and (string? prior)
             (string? d))
      (conj (vec (butlast accum))
            (str prior d))
      (conj accum d))))

(defn str->text-nodes [s]
  (if (string? s)
    {:text s}
    s))

(defn aggregate-groups [accum]
  (->> (reduce concatenate-adjacent-strings [] accum)
       (remove nil?)
       (mapv str->text-nodes)))

(defn split-around [s target]
  (let [i (string/index-of s target)
        left (subs s 0 i)
        right (subs s (+ i (count target)))]
    [left right]))

(defn partition-trio [contents open mid close]
  (let [[left _ right] (partition-by #(= mid %) contents)]
    {:left (->> left (remove #{open}) aggregate-groups)
     :right (->> right (remove #{close}) aggregate-groups)}))

(defn extract-node [accum open close]
  (let [kind (type-for close)]
    (if-let [mid (get-in group-trios [close :middle])]
      {kind (partition-trio accum open mid close)}
      {kind (aggregate-groups (butlast accum))})))

(defn has-trio-group-middle [coll middle]
  (not (nil? (some #(= middle %) coll))))

(defn at-group-start? [c open close coll]
  (and (= c open)
       (>= (count coll) 2)
       (if (contains? group-trios close)
         (has-trio-group-middle coll (middle-token-for close))
         true)))

(defn parse-node [stack end]
  (let [target (open-for end)]
    (loop [stack (conj stack end)
           accum ()]
      (if-let [c (first stack)]
        (if (at-group-start? c target end accum)
          {:node (extract-node accum target end) :unused (rest stack)}
          (recur (rest stack) (conj accum c)))
        {:unused (apply str accum)}))))

(defn finalize-parse [accum]
  (if (= (count accum) 1)
    (first accum)
    (let [aggregate (aggregate-groups (reverse accum))]
      (if (= (count aggregate) 1)
        (first aggregate)
        {:tree aggregate}))))

(defn tokens->tree [tokens]
  (loop [accum ()
         tokens (reverse tokens)]
    (if-let [v (first tokens)]
      (cond
        (group-end? v) (let [{:keys [node unused]} (parse-node accum v)]
                         (if (nil? node)
                           (recur (conj accum v) (rest tokens))
                           (if-not (empty? unused)
                             (recur (conj unused node) (rest tokens))
                             (recur (list node) (rest tokens)))))
        :default (recur (conj accum v) (rest tokens)))
      (finalize-parse accum))))

(defn str->tokens [s]
  (loop [s s
         stack ()]
    (if (empty? s)
      stack
      (if (and (>= (count s) 2) (contains? two-char-tokens (subs s 0 2)))
        (recur (subs s 2)
               (conj stack (subs s 0 2)))
        (recur (subs s 1)
               (conj stack (subs s 0 1)))))))

(defn str->tree [s]
  (-> s str->tokens tokens->tree))

(defmulti tree->str (comp first keys))

(defmethod tree->str :text [tree] (:text tree))
(defmethod tree->str :tree [tree] (apply str (map tree->str (:tree tree))))
(defmethod tree->str :default [tree]
  (let [kind (first (keys tree))
        close (close-for-type kind)
        open (open-for close)]
    (str open (apply str (map tree->str (kind tree))) close)))

(def parse str->tree)
