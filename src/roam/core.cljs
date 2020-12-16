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

(defn text-on-change [source]
  (fn [e]
    (reset! source (.. e -target -value))))

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
       [:pre (let [text @text]
               (with-out-str
                 (pprint (time (parser/parse text)))))]
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

       [:pre {:style {:overflow :auto :word-wrap :break-word}}
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
                (println e)))))
        ]])))

(defn mount-root []
  (rdom/render [demo]
               (js/document.getElementById "app-root")))
