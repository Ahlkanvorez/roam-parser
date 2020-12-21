(ns roam.core
  (:require [cljs.pprint :refer [pprint]]
            [cljs.reader :as edn]
            [reagent.core :as r]
            [reagent.dom :as rdom]
            [roam.parser :as parser]))

(def initial-text "The parser can parse all of Roam's major syntax at least [[Nested [[Links]]]] and ^^**bold highlights**^^ and `[html roam]([[Aliases]])`all the ones we haven't done yet as well. Specifically ```javascript

Aliases inside aliases
>   
  [![img](image-as-alias.com)](www.roamresearch.com)

{{{{curly braces}} in{{side}} of {{curly braces}}}}


```")

(def initial-update-path "[:tree 0]")
(def initial-update-value "{:text \"You can also do in-place updates with update-in, assoc-in, etc\"}")

(def pre-style {:style {:overflow :none :word-wrap :break-word}})

(defn text-on-change [source]
  (fn [e]
    (reset! source (.. e -target -value))))

(defn rand-char []
  (rand-nth "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789 \n\t-_"))

(defn rand-roam-type []
  (rand-nth (vals parser/type-for)))

(defn rand-roam-node [text-length nesting-probability]
  (if (<= (rand) nesting-probability)
    (let [kind (rand-roam-type)
          close (parser/close-for-type kind)
          open (parser/open-for close)]
      (if (contains? parser/group-trios close)
        (let [left (rand-roam-node text-length nesting-probability)
              right (rand-roam-node text-length  nesting-probability)
              separator (parser/middle-token-for close)]
          (str (apply str open) left (apply str separator) right (apply str close)))
        (let [inner (rand-roam-node text-length  nesting-probability)]
          (str (apply str open) inner (apply str close)))))
    (apply str (take (rand-int text-length) (repeatedly rand-char)))))

(defn random-roam-string [top-level-nodes text-length nesting-probability]
  (apply str (take (rand-int top-level-nodes)
                   (repeatedly #(rand-roam-node text-length
                                                nesting-probability)))))

(defn benchmark []
  (let [benchmark-runtime (r/atom "")
        max-top-level-nodes (r/atom 20)
        max-text-length (r/atom 1000)
        nesting-probability (r/atom 0.5)
        do-benchmark
        (fn []
          (if (>= 1.0 @nesting-probability)
            (js/alert "You can only have nesting probabilities < 1.")
            (let [test-data (take 1000
                                  (repeatedly #(random-roam-string
                                                @max-top-level-nodes
                                                @max-text-length
                                                @nesting-probability)))]
              (reset! benchmark-runtime
                      (with-out-str
                        (time
                         (doseq [datum test-data]
                           (parser/parse datum))))))))]
    (fn []
      [:div
       [:h2 "Some Benchmarks"]
       [:p
        "Clicking the button below will generate 1000 random Roam "
        "strings with the settings below, and fully parse them, then "
        "display the total runtime."]
       
       [:input {:type :button :value "Run Benchmark"
                :on-click do-benchmark}]
       (when @benchmark-runtime
         [:pre @benchmark-runtime])
       (let [example (random-roam-string @max-top-level-nodes
                                         @max-text-length
                                         @nesting-probability)]
         [:div
          [:p "Below is a sample string generated with the selected settings:"]
          [:pre pre-style example]
          [:p "And here's its parse string"]
          [:pre pre-style
           (with-out-str
             (pprint (time (parser/parse example))))]])])))

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
        " with arguments from the two inputs below, in that order, and serializes the result back to the shown roam string. These updates can be done with the built-in clojure update, update-in, assoc, assoc-in, etc functions, because the tree is simply a clojure map."]
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
                    parser/tree->str)))
              (catch :default e
                (println "Error parsing:")
                (println e)))))]
       [:hr]
       [benchmark]])))

(defn mount-root []
  (rdom/render [demo]
               (js/document.getElementById "app-root")))
