(ns roam.core
  (:require [reagent.dom :as rdom]
            [roam.views.benchmark :as benchmark]
            [roam.views.demo :as demo]))

(defn mount-root []
  (rdom/render [:div {:class :container-fluid}
                [demo/view]
                [benchmark/view]]
               (js/document.getElementById "app-root")))
