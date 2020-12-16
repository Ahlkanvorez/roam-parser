(ns roam.core
  (:require [clojure.string :as string]))

(def group-complement
  {(seq "]]") (seq "[[")
   (seq "))") (seq "((")
   (seq "}}") (seq "{{")
   (seq "$$") (seq "$$")
   (seq "^^") (seq "^^")
   (seq "**") (seq "**")
   (seq "__") (seq "__")
   (seq "```") (seq "```")
   (seq "`") (seq "`")
   (seq "\"") (seq "\"")
   (seq ")") (seq "[")})

(def three-char-tokens (set (filter #(= 3 (count %)) (keys group-complement))))

(def group-trios
  {(seq ")") {:open (seq "[")
              :middle (seq "](")
              :close (seq ")")}})

(defn middle-token-for [trio-close-token]
  (get-in group-trios [trio-close-token :middle]))

(def open-for (partial get group-complement))

(def type-for {(seq "]]") :link
               (seq "))") :ref
               (seq "}}") :roam-render
               (seq "$$") :latex
               (seq "^^") :highlight
               (seq "**") :bold
               (seq "__") :italic
               (seq "```") :block-quote
               (seq "`") :syntax-quote
               (seq "\"") :quote
               (seq ")") :alias})

(def close-for-type (apply merge (map (fn [[k v]] {v k}) type-for)))

(defn concatenate-adjacent-strings [accum d]
  (let [prior (or (last accum) "")]
    (if (and (or (char? prior) (string? prior)) (char? d))
      (conj (vec (butlast accum)) (str d prior))
      (conj accum d))))

(defn str->text-nodes [s]
  (cond (string? s) {:text (apply str (reverse s))}
        (char? s) {:text (str s)}
        :default s))

(defn aggregate-groups [accum]
  (->> accum
       (reduce concatenate-adjacent-strings [])
       (remove nil?)
       (mapv str->text-nodes)))

(defn split-seq [s t]
  (loop [left []
         s s]
    (cond (empty? s) [left []]
          (= (take (count t) s) t) [left (drop (count t) s)]
          :default (recur (conj left (first s)) (rest s)))))

(defn partition-trio [contents open mid close]
  (let [[left right] (split-seq contents mid)]
    {:left (->> left (remove #{open}) aggregate-groups)
     :right (->> right (remove #{close}) aggregate-groups)}))

(defn extract-node [accum open close]
  (let [kind (type-for close)]
    (if-let [mid (get-in group-trios [close :middle])]
      {kind (partition-trio (drop-last (count close) accum) open mid close)}
      {kind (aggregate-groups (drop-last (count close) accum))})))

(defn has-trio-group-middle [coll mid]
  (cond (empty? coll) false
        (= (take 2 coll) mid) true
        :default (recur (rest coll) mid)))

(defn at-node-start? [c open close coll]
  (and (= c open)
       (>= (count coll) 2)
       (if (contains? group-trios close)
         (has-trio-group-middle coll (middle-token-for close))
         true)))

(defn node-terminus [s endpoint?]
  (loop [tokens [[(take 3 s) 3] [(take 2 s) 2] [(take 1 s) 1] [(first s) 1]]]
    (when-let [[token size] (first tokens)]
      (if (endpoint? token)
        {:token token :size size}
        (recur (rest tokens))))))

(defn node-start [tokens open close coll]
  (node-terminus tokens #(at-node-start? % open close coll)))

(defn terminal-token-with-prior-start? [close coll]
  (when-let [open (open-for close)]
    (loop [coll coll]
      (cond (empty? coll) false
            (or (= (take (count open) coll) open)
                (= (first coll) open)) true
            :default (recur (rest coll))))))

(defn node-end [s coll]
  (node-terminus s #(terminal-token-with-prior-start? % coll)))

(defn parse-node [stack end]
  (let [target (open-for end)]
    (loop [stack (into stack end)
           accum ()]
      (if (empty? stack)
        {:unused (apply str (reverse accum))}
        (if-let [token (node-start stack target end accum)]
          {:node (extract-node accum target end)
           :unused (drop (:size token) stack)}
          (recur (rest stack) (conj accum (first stack))))))))

(defn finalize-parse [accum]
  (if (= (count accum) 1)
    (first accum)
    (let [aggregate (aggregate-groups (reverse accum))]
      (if (= (count aggregate) 1)
        (first aggregate)
        {:tree aggregate}))))

(defn tokens->tree [tokens]
  (loop [accum ()
         tokens tokens]
    (if (empty? tokens)
      (finalize-parse accum)
      (if-let [terminal-token (node-end tokens accum)]
        (let [{:keys [token size]} terminal-token
              {:keys [node unused]} (parse-node accum token)
              next-tokens (drop size tokens)]
          (if (nil? node)
            (recur (into accum token) next-tokens)
            (if-not (empty? unused)
              (recur (conj unused node) next-tokens)
              (recur (list node) next-tokens))))
        (recur (conj accum (first tokens)) (rest tokens))))))

(defn str->tokens [s]
  (loop [tokens []
         s (seq s)]
    (cond (contains? three-char-tokens (take 3 s))
          (recur (conj tokens (take 3 s)) (drop 3 s))
          (empty? s) tokens
          :default (recur (conj tokens (first s)) (rest s)))))

(defn str->tree [s]
  (-> s str->tokens tokens->tree))

(defmulti tree->str (comp first keys))

(defmethod tree->str :text [tree] (:text tree))
(defmethod tree->str :tree [tree] (apply str (map tree->str (:tree tree))))
(defmethod tree->str :default [tree]
  (let [kind (first (keys tree))
        close (close-for-type kind)
        open (open-for close)]
    (str (apply str open)
         (apply str (map tree->str (kind tree)))
         (apply str close))))

(def parse str->tree)
