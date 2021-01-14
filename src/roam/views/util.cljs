(ns roam.views.util)

(defn prepended-input [label input]
  [:div {:class "input-group mb-3"}
   [:div {:class "input-group-prepend"}
    [:label {:class "input-group-text"} label]]
   input])

(def pre-style
  {:style {:border-left "3px solid #CCC"
           :padding-left "10px"
           :margin-top "10px"
           :white-space :pre-wrap}})

