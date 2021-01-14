(ns roam.views.benchmark
  (:require [cljs.pprint :refer [pprint]]
            [reagent.core :as r]
            [roam.parser :as parser]
            [roam.rand :refer [rand-tree]]
            [roam.views.util :refer
             [debounce prepended-input pre-style]]))

(defn sum-with [f coll]
  (transduce (map f) + 0 coll))

(defn average-with [f coll]
  (/ (sum-with f coll) (count coll)))

(defn median [coll]
  (nth coll (quot (count coll) 2)))

(defn runtime-summary [benchmark-runtimes]
  [:table {:class "table"}
   [:thead
    [:tr
     [:th {:scope "col"} ""]
     [:th {:scope "col"} "ms"]]]
   [:tbody
    [:tr
     [:th {:scope "row"} "Total"]
     [:td (sum-with (comp js/parseFloat :runtime) @benchmark-runtimes)]]
    [:tr
     [:th {:scope "row"} "Average"]
     [:td (average-with (comp js/parseFloat :runtime) @benchmark-runtimes)]]
    [:tr
     [:th {:scope "row"} "Median"]
     [:td (:runtime (median @benchmark-runtimes))]]]])

(defn runtime-samples [benchmark-runtimes]
  [:table {:class "table"}
   [:thead
    [:tr
     [:th "Time (ms)"]
     [:th {:scope "col"} "Test data (10 slowest parses only)"]]]
   [:tbody
    (for [[idx case]
          (take 10
                (map vector (range)
                     (sort-by (comp js/parseFloat :runtime)
                              >
                              @benchmark-runtimes)))]
      [:tr {:key idx}
       [:td [:pre (:runtime case)]]
       [:td
        [:blockquote {:class "blockquote"
                      :style {:display :inline-block}}
         (:s case)]
        [:pre pre-style
         (with-out-str
           (cljs.pprint/pprint (:result case)))]]])]])

(defn make-benchmark [word-count benchmark-runtimes]
  (fn []
    (let [test-data
          (take 1000 (repeatedly #(rand-tree @word-count)))
          results (atom [])]
      (doseq [datum test-data]
        (let [r (atom nil)
              s (parser/tree->str datum)
              t (->> (with-out-str
                       (time (reset! r (parser/parse s))))
                     (re-find #"\d+\.\d+"))]
          (swap! results conj
                 {:runtime t :datum datum :s s :result @r})))
      (reset! benchmark-runtimes @results))))

(defn rand-sample [word-count]
  (let [tree (rand-tree @word-count)
        example (parser/tree->str tree)]
    [:div
     [:p "Below is a sample string generated with the selected settings:"]
     [:div {:style {:display :flex :flex-direction :column}}
      [:blockquote {:class "blockquote"} example]
      [:div
       [:pre pre-style
        (with-out-str
          (pprint (time (parser/parse example))))]]]]))

(defn view []
  (let [benchmark-runtimes (r/atom [])
        word-count (r/atom 5)
        do-benchmark
        (debounce
         (fn [_e]
           ((make-benchmark word-count benchmark-runtimes)))
         500)]
    (fn []
      [:div
       [:h1 {:class "display-4"} "Some Benchmarks"]
       [prepended-input
        "Minimum Words per Node"
        [:input {:type :number :value @word-count :min 1
                 :on-change #(reset! word-count
                                     (js/parseFloat (.. % -target -value)))}]]
       [:button {:class "btn btn-primary" :on-click do-benchmark}
        (str "Parse 1000 random strings")]
       [rand-sample word-count]
       (when-not (empty? @benchmark-runtimes)
         [:div
          [runtime-summary benchmark-runtimes]
          [runtime-samples benchmark-runtimes]])])))
