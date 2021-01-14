(ns roam.views.demo
  (:require [cljs.reader :as edn]
            [cljs.pprint :refer [pprint]]
            [reagent.core :as r]
            [roam.parser :as parser]
            [roam.views.util :refer [prepended-input pre-style]]))

(def initial-text "The parser can parse all of Roam's major syntax at least [[Nested [[Links]]]] and ^^**bold highlights**^^ and `[html roam]([[Aliases]])`all the ones we haven't done yet as well. Specifically ```javascript

Aliases inside aliases
>   
  [![img](image-as-alias.com)](www.roamresearch.com)

{{{{curly braces}} in{{side}} of {{curly braces}}}}


```")

(def initial-update-path "[:tree 7 :block 0 :tree 1 :alias 0 :tree 1 :alias 0 :text]")
(def initial-update-value "\"You can also do in-place updates with update-in, assoc-in, etc. \"")

(defn text-on-change [source]
  (fn [e] (reset! source (.. e -target -value))))

(defn intro []
  [:div
   [:h1 {:class "display-3"} "Roam Parser"]
   [:a {:href "https://github.com/Ahlkanvorez/roam-parser"
        :target "_blank"
        :rel "noreferrer noopener"}
    "View Source"]
   [:p {:class "lead"}
    "This parser is written in pure ClojureScript, with no "
    "dependencies other than Reagent for the minimal UI. Roam "
    "strings, the textual markup language used by "
    [:a {:href "https://roamresearch.com"
         :target "_blank"
         :rel "noreferrer noopener"} "Roam Research"]
    " are parsed into EDN, a tree of native clojure maps, vectors, "
    "and strings; this means the tree can be manipulated trivially "
    "using the wonderful built-in Clojure functions for those data "
    "structures."]])

(defn parse-example [text]
  [:div
   [:p
    "The listed elapsed times are the actual runtimes of the "
    "parsing & serialization work done in your browser. Changing "
    "the the top-most text area will result in all parsed trees "
    "being updated, whereas updating either of the bottom two for "
    "changing the in-place update only recalculates the bottom-most "
    "text."]
   [:textarea {:class "form-control"
               :rows 11
               :value @text :on-change (text-on-change text)}]
   [:pre pre-style
    [:code (with-out-str (pprint (time (parser/parse @text))))]]])

(defn update-example [text update-path update-value]
  [:div
   [:h1 {:class "display-4"} "Updating the Tree"]
   [:p
    "The following parses the above input, updates the tree representation using "
    [:a {:href "https://clojuredocs.org/clojure.core/assoc-in"
         :target "_blank"
         :rel "noreferrer noopener"}
     [:code "assoc-in"]]
    " with arguments from the two inputs below, in that order, and"
    " serializes the result back to the shown roam string. These"
    " updates can be done with the built-in clojure functions "
    [:code "update"] ", " [:code "update-in"] ", "
    [:code "assoc"] ", " [:code "assoc-in"] ", etc."]
   [prepended-input
    "Path"
    [:input {:style {:flex "1 0"}
             :type :text
             :value @update-path
             :on-change (text-on-change update-path)}]]
   [prepended-input
    "Value"
    [:input {:style {:flex "1 0"}
             :type :text
             :value @update-value
             :on-change (text-on-change update-value)}]]

   [:pre pre-style
    (with-out-str (cljs.pprint/pprint (parser/parse @text)))]
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
            (println e)))))]])

(defn view []
  (let [text (r/atom initial-text)
        update-path (r/atom initial-update-path)
        update-value (r/atom initial-update-value)]
    [:div
     [intro]
     [parse-example text]
     [update-example text update-path update-value]]))
