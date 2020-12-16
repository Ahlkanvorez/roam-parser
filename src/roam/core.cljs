(ns roam.core
  (:require [reagent.core :as r]
            [reagent.dom :as rdom]))

(defn mount-root []
  (rdom/render [:h1 "Parser"]
               (js/document.getElementById "app-root")))
