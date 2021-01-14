(ns roam.core
  (:require [reagent.dom :as rdom]
            [roam.views.benchmark :as benchmark]
            [roam.views.custom :as custom]
            [roam.views.demo :as demo]
            ["mathjs" :as math]))

(defn mount-root []
  (rdom/render [:div {:class :container-fluid}
                [demo/view]
                [custom/view]
                [benchmark/view]]
               (js/document.getElementById "app-root")))
