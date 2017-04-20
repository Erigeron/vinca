
(ns client.comp.container
  (:require [hsl.core :refer [hsl]]
            [respo-ui.style :as ui]
            [respo.alias :refer [create-comp div span]]
            [respo.comp.space :refer [comp-space]]
            [respo.comp.text :refer [comp-text comp-code]]))

(def demo-program ["+" "1" ["+" "2" "3"]])

(def comp-container
  (create-comp
   :container
   (fn [store]
     (fn [cursor]
       (div {:style (merge ui/global ui/fullscreen)} (comp-code (pr-str demo-program) nil))))))
