(ns roam.parser.tree)

(deftype Node [kind children]
  Object
  (toString [_this] (str "#Node{" kind " " children "}"))
  (equiv [this other] (-equiv this other))

  IPrintWithWriter
  (-pr-writer [this writer _opts] (-write writer (str this)))

  IEquiv
  (-equiv [_this other]
    (and (instance? Node other)
         (= kind (.-kind other))
         (= children (.-children other))))

  ICounted
  (-count [_this] (count children))

  ICollection
  (-conj [_this o] (Node. kind (conj children o)))

  IIndexed
  (-nth [_this n] (nth children n))
  (-nth [_this n not-found] (nth children n not-found))

  ILookup
  (-lookup [this k]
    (cond (nat-int? k) (nth this k)
          (keyword? k) (-find this k)))
  (-lookup [this k not-found] (or (-lookup this k) not-found))

  ISeq
  (-first [_this] kind)
  (-rest [_this] children)

  ISeqable
  (-seq [o] [kind children])

  IAssociative
  (-contains-key? [_this k]
    (or (keyword? k) (and (nat-int? k) (< k (count children)))))
  (-assoc [this k v]
    (cond (keyword? k)
          (let [idx (->> (map vector children (range))
                         (filter #(= k (.-kind (first %))))
                         first second)]
            (Node. kind
                   (into (conj (vec (take idx children)) v)
                         (drop (inc idx) children))))
          (nat-int? k) (Node. kind (assoc children k v))))

  IMap
  (-dissoc [this k]
    (if (-contains-key? this k)
      (Node. kind (into (vec (take k children))
                        (drop (inc k) children)))
      this))

  IFind
  (-find [_this k] (first (filter #(= k (first %)) children))))

(defn map-tree
  ([f tree] (map-tree f [] tree))
  ([f path tree]
   (cond (string? tree) (f path tree)
         (vector? tree) (vec (map-indexed
                              (fn [idx child]
                                (map-tree f (conj path idx) child))
                              tree))
         (not (nil? tree))
         (let [kind (.-kind tree)
               kids (map-tree f (conj path kind) (.-children tree))]
           (f (conj path kind)
              (if (= kids (.-children tree))
                tree
                (Node. kind kids)))))))

(defn map-leaves [f tree]
  (map-tree (fn [path node]
              (if (string? node)
                (f path node)
                node))
            tree))
