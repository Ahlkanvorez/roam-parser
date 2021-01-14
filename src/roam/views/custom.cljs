(ns roam.views.custom
  (:require [roam.parser :as parser]
            [roam.views.util :refer [pre-style]]))

(def text "{math: 12.7 cm to inch}
{math: pi \\* cos(45 deg) \\^ 2}
{math: 9 / 3 + 2i}
{math: det([-1, 2; 3, 1])}")

(defn view []
  [:div
   [:h1 {:class "display-4"} "Custom Evaluation"]
   [:p "Evaluators can easily be added as a second stage after parsing"
    " for arbitrary extensions. The following " [:code ":code"]
    " blocks start with " [:code "math:"] ", so they're evaluated by"
    " math.js; single char operators used by other parser rules are"
    " escaped:"]
   [:pre pre-style text]
   [:p "The above evaluates to:"]
   [:pre pre-style
    (with-out-str (try (cljs.pprint/pprint (parser/parse text))
                       (catch :default e
                         (println "Parse failed with exception " e))))]])
