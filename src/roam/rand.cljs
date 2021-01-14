(ns roam.rand
  (:require [clojure.string :as string]
            [roam.parser.roam :as roam]))

(def rand-word
  (let [word-bank
        (-> ;; vide Ovidi Metamorphoseon I.i-xxxi
         " In nova fert animus mutatas dicere formas
corpora; di, coeptis (nam vos mutastis et illas)
adspirate meis primaque ab origine mundi
ad mea perpetuum deducite tempora carmen!
     Ante mare et terras et quod tegit omnia caelum
unus erat toto naturae vultus in orbe,
quem dixere chaos: rudis indigestaque moles
nec quicquam nisi pondus iners congestaque eodem
non bene iunctarum discordia semina rerum.
nullus adhuc mundo praebebat lumina Titan,
nec nova crescendo reparabat cornua Phoebe,
nec circumfuso pendebat in aere tellus
ponderibus librata suis, nec bracchia longo
margine terrarum porrexerat Amphitrite;
utque erat et tellus illic et pontus et aer,
sic erat instabilis tellus, innabilis unda,
lucis egens aer; nulli sua forma manebat,
obstabatque aliis aliud, quia corpore in uno
frigida pugnabant calidis, umentia siccis,
mollia cum duris, sine pondere, habentia pondus.
     Hanc deus et melior litem natura diremit.
nam caelo terras et terris abscidit undas
et liquidum spisso secrevit ab aere caelum.
quae postquam evolvit caecoque exemit acervo,
dissociata locis concordi pace ligavit:
ignea convexi vis et sine pondere caeli
emicuit summaque locum sibi fecit in arce;
proximus est aer illi levitate locoque;
densior his tellus elementaque grandia traxit
et pressa est gravitate sua; circumfluus umor
ultima possedit solidumque coercuit orbem."
         (string/split #"\W+")
         (as-> words (remove empty? words)))]
    (fn [] (rand-nth word-bank))))

(def types (concat (map first roam/type->open-close)
                   (map first roam/type->open-mid-close)))

(defn rand-roam-type [exclude]
  (rand-nth (remove exclude types)))

(defn rand-sentence [word-count]
  (->> (repeatedly rand-word)
       (take word-count)
       (interpose " ")
       (apply str)))

(defn rand-text-node [word-count] {:text [(rand-sentence word-count)]})

(defn rand-node
  "Generate a random roam node, ensuring syntax and block nodes
  do not nest."
  [word-count exclude]
  (letfn [(rand-kids [t]
            (let [exclude
                  (cond exclude exclude
                        (or (= t :syntax)
                            (= t :block))
                        #{:syntax :block})]
              (if (< (rand) 0.2)
                {:tree (vec (take (inc (rand-int 9))
                                  (repeatedly
                                   #(rand-node word-count
                                               exclude))))}
                (rand-node word-count exclude))))]
    (if (< (rand) 0.2)
      (let [t (rand-roam-type exclude)]
        (if (contains? roam/type->open-mid-close t)
          {t [(rand-kids t) (rand-kids t)]}
          {t [(rand-kids t)]}))
      (rand-text-node word-count))))

(defn rand-tree [word-count]
  (clojure.walk/prewalk
   (fn [n]
     (if (and (map? n) (= :tree (first (first n))))
       {:tree
        (vec
         (reduce
          (fn [accum n]
            (let [m (last accum)]
              (if (and m
                       (= :text (first (first m)))
                       (= :text (first (first n))))
                (conj (pop accum)
                      {:text
                       [(str (first (second (first m)))
                             " "
                             (first (second (first n))))]})
                (conj accum n))))
          []
          (second (first n))))}
       n))
   (rand-node word-count #{})))
