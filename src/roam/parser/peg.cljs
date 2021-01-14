(ns roam.parser.peg
  (:require-macros [roam.parser.peg-util :refer [defparser]]))

(deftype Result [v s]
  Object
  (toString [_this] (str "#Result{" v " " s "}"))
  (equiv [this other] (-equiv this other))

  IPrintWithWriter
  (-pr-writer [this writer _opts] (-write writer (str this)))

  IEquiv
  (-equiv [_this other]
    (and (instance? Result other)
         (= v (.-v other)) (= s (.-s other))))

  ISeq
  (-first [_this] (first s))
  (-rest [_this] (rest s))

  ISeqable
  (-seq [_this] (seq s)))

(def path (atom []))

(defrecord Parser [parse-fn args]
  IFn
  (-invoke [this s] (parse-fn s args)))

(defparser char-parser [s c]
  (when (= (first s) c)
    (Result. nil (rest s))))

(defparser ordered-choice-parser [s options]
  (loop [options options]
    (when-let [o (first options)]
      (if-let [rem (o s)]
        rem
        (recur (rest options))))))

(defparser sequence-parser [s sequence]
  (loop [sequence sequence
         s s
         results []]
    (if-let [a (first sequence)]
      (when-let [^Result rem (a s)]
        (recur (rest sequence)
               (.-s rem)
               (if (.-v rem)
                 (conj results (.-v rem))
                 results)))
      (Result. results s))))

(defparser and-parser [s p]
  (when (p s)
    (Result. nil s)))

(defparser not-parser [s p]
  (when-not (p s)
    (Result. nil s)))

(defparser plus-parser [s p]
  (loop [s s
         results []]
    (if-let [^Result rem (p s)]
      (recur rem (conj results (.-v rem)))
      (when (seq results)
        (Result. results s)))))

(defparser star-parser [s p]
  (loop [s s
         results []]
    (if-let [^Result rem (p s)]
      (recur rem (conj results (.-v rem)))
      (Result. results s))))

(defparser optional-parser [s p]
  (if-let [^Result rem (p s)]
    (Result. (.-v rem) rem)
    s))

(defparser class-parser [s c]
  (when-let [t (first s)]
    (when (c t)
      (Result. t (rest s)))))

(defparser range-parser [s r]
  (when-let [t (first s)]
    (when (<= (.charCodeAt (first r) 0)
              (.charCodeAt t 0)
              (.charCodeAt (second r) 0))
      (Result. t (rest s)))))

(defparser string-parser [s e]
  (let [c (count e)
        t (take c s)]
    (when (= t (seq e))
      (Result. nil (drop c s)))))

(defparser any-parser [s]
  (Result. (first s) (next s)))

(declare compile-parser)

(defparser pattern-parser [s patterns]
  (let [desired (first patterns)
        patterns (second patterns)]
    (swap! path conj desired)
    (let [r (when (seq s)
              (when-let [p (compile-parser patterns (get patterns desired))]
                (when-let [^Result res (p s)]
                  (Result. {desired (.-v res)} (.-s res)))))]
      (swap! path butlast)
      r)))

(def compile-parser
  (memoize
   (fn [patterns A]
     (cond
       (= '- A) (any-parser)
       (keyword? A) (pattern-parser [A patterns])
       (char? A) (char-parser A)
       (string? A) (string-parser A)
       (map? A)
       (let [[pattern op] (first A)]
         (case op
           | (ordered-choice-parser
              (map (partial compile-parser patterns) pattern))
           . (sequence-parser
              (map (partial compile-parser patterns) pattern))
           + (plus-parser (compile-parser patterns pattern))
           * (star-parser (compile-parser patterns pattern))
           - (range-parser pattern)
           ? (optional-parser (compile-parser patterns pattern))
           ! (not-parser (compile-parser patterns pattern))
           & (and-parser (compile-parser patterns pattern))))
       (or (set? A) (fn? A)) (class-parser A)))))

(defn compile-all [patterns]
  (->> patterns
       (map (fn [[n p]] {n (compile-parser patterns p)}))
       (apply merge)))
