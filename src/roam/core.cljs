(ns roam.core
  (:require [cljs.pprint :refer [pprint]]
            [cljs.reader :as edn]
            [reagent.core :as r]
            [reagent.dom :as rdom]
            [roam.parser :as parser]
            [roam.parser.tree :as tree]
            [roam.parser.syntax :as syntax]))

(def initial-text "The parser can parse all of Roam's major syntax at least [[Nested [[Links]]]] and ^^**bold highlights**^^ and `[html roam]([[Aliases]])`all the ones we haven't done yet as well. Specifically ```javascript

Aliases inside aliases
>   
  [![img](image-as-alias.com)](www.roamresearch.com)

{{{{curly braces}} in{{side}} of {{curly braces}}}}


```")

(def initial-update-path "[:text 0]")
(def initial-update-value "\"You can also do in-place updates with update-in, assoc-in, etc. \"")

(def pre-style {:style {:overflow :none :word-wrap :break-word}})

(defn text-on-change [source]
  (fn [e]
    (reset! source (.. e -target -value))))

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
         (clojure.string/split #"\W+")
         (as-> words (remove empty? words)))]
    (fn [] (rand-nth word-bank))))

(defn rand-roam-type []
  (rand-nth (map #(.-name %) syntax/higher-rules)))

(defn rand-sentence [word-count]
  (->> (repeatedly rand-word)
       (take (inc (rand-int (dec word-count))))
       (interpose " ")
       (apply str)))

(defn rand-text-node [word-count]
  (tree/Node. :text [(rand-sentence word-count)]))

(defn rand-roam-node [word-count max-height max-width]
  (when (> max-height 0)
    (let [kind (rand-roam-type)
          kids
          (loop [children []
                 width (rand-int max-width)]
            (if (> width 0)
              (let [child (rand-roam-node word-count
                                          (dec max-height)
                                          max-width)]
                (if child
                  (recur (conj children child)
                         (dec width))
                  (recur (conj children (rand-text-node word-count))
                         (dec width))))
              (if (empty? children)
                [(rand-text-node word-count)]
                children)))]
      (parser/node->str (tree/Node. kind (remove nil? kids))))))

(defn benchmark []
  (let [benchmark-runtimes (r/atom [])
        max-words-per-node (r/atom 30)
        max-width (r/atom 4)
        max-height (r/atom 4)
        do-benchmark
        (fn []
          (let [test-data (take 1000
                                (repeatedly #(rand-roam-node
                                              @max-words-per-node
                                              @max-width
                                              @max-height)))
                results (atom [])]
            (doseq [datum test-data]
              (let [t (->> (with-out-str (time (parser/parse datum)))
                           (re-find #"\d+\.\d+"))]
                (swap! results conj {:runtime t :data datum})))
            (reset! benchmark-runtimes @results)))]
    (fn []
      [:div
       [:h2 "Some Benchmarks - using str"]
       [:div
        [:input {:type :number :value @max-words-per-node :min 1
                 :on-change #(reset! max-words-per-node (js/parseFloat (.. % -target -value)))}]
        [:input {:type :number :value @max-width :min 1
                 :on-change #(reset! max-width (js/parseFloat (.. % -target -value)))}]
        [:input {:type :number :value @max-height :min 1
                 :on-change #(reset! max-height (js/parseFloat (.. % -target -value)))}]]
       [:p
        "Clicking the button below will generate 1000 random Roam "
        "strings with the settings above, and fully parse them, then "
        "display the total runtime."]
       
       [:input {:type :button :value "Run Benchmark"
                :on-click do-benchmark}]
       (let [example (rand-roam-node @max-words-per-node
                                     @max-height
                                     @max-width)]
         [:div
          [:p "Below is a sample string generated with the selected settings:"]
          [:code pre-style example]
          [:p "And here's its parse string"]
          [:div
           [:pre pre-style
            (with-out-str
              (pprint (time (parser/parse example))))]]])
       (when-not (empty? @benchmark-runtimes)
         [:div
          [:p "Total Runtime: " (->> @benchmark-runtimes
                                     (map (comp js/parseFloat :runtime))
                                     (reduce + 0)) " ms"]
          [:table
           [:thead
            [:tr
             [:th "Time (ms)"]
             [:th "Test data"]]]
           [:tbody
            (for [[idx case] (map vector (range)
                                  (sort-by (comp js/parseFloat :runtime)
                                           >
                                           @benchmark-runtimes))]
              [:tr {:key idx}
               [:td (:runtime case)]
               [:td (:data case)]])]]]
         )])))

(defn demo []
  (let [text (r/atom initial-text)
        update-path (r/atom initial-update-path)
        update-value (r/atom initial-update-value)]
    (fn []
      [:div
       [:h1 "Roam Parser"]
       [:p
        "This parser is written in pure ClojureScript, with no "
        "dependencies other than Reagent for the minimal UI. Roam "
        "strings, the textual markup language used by "
        [:a {:href "https://roamresearch.com"} "Roam Research"]
        " are parsed into a tree of native clojure maps, vectors, and "
        "strings; this means the tree can be manipulated trivially "
        "using the wonderful built-in Clojure functions for those data "
        "structures."]
       [:p
        "The listed elapsed times are the actual runtimes of the "
        "parsing & serialization work done in your browser. Changing "
        "the the top-most text area will result in all parsed trees "
        "being updated, whereas updating either of the bottom two for "
        "changing the in-place update only recalculates the bottom-most "
        "text."]
       [:textarea {:style {:width "100%" :max-width "100%"}
                   :value @text :on-change (text-on-change text)}]
       [:pre pre-style
        (let [text @text]
          (with-out-str
            (pprint (time (parser/parse text)))))]
       [:hr]
       [:h2 "Updating the Tree"]
       [:p
        "The following parses the above input, updates the tree representation using "
        [:a {:href "https://clojuredocs.org/clojure.core/assoc-in"} "assoc-in"]
        " with arguments from the two inputs below, in that order, and serializes the result back to the shown roam string. These updates can be done with the built-in clojure update, update-in, assoc, assoc-in, etc functions, since the tree type implements all the necessary protocols."]
       [:div {:style {:display :flex}}
        [:label {:style {:flex "0 0 50px"}} "Path"]
        [:input {:style {:flex "1 0"}
                 :type :text :value @update-path :on-change (text-on-change update-path)}]]
       [:div {:style {:display :flex}}
        [:label {:style {:flex "0 0 50px"}} "Value"]
        [:input {:style {:flex "1 0"}
                 :type :text :value @update-value :on-change (text-on-change update-value)}]]

       [:pre pre-style
        (let [text @text
              path @update-path
              value @update-value]
          (with-out-str
            (try
              (println
               (time
                (-> text
                    parser/parse
                    (assoc-in (edn/read-string path) (edn/read-string value))
                    parser/node->str)))
              (catch :default e
                (println "Error parsing:")
                (println e)))))]
       [:hr]
       [benchmark]])))

(defn mount-root []
  (rdom/render [demo]
               (js/document.getElementById "app-root")))
