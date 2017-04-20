
(ns client.comp.container
  (:require [hsl.core :refer [hsl]]
            [respo-ui.style :as ui]
            [respo.alias :refer [create-comp div span]]
            [respo.comp.space :refer [comp-space]]
            [respo.comp.text :refer [comp-text]]))

(def comp-container
  (create-comp
   :container
   (fn [store]
     (fn [cursor] (div {:style (merge ui/global ui/fullscreen)} (comp-text "Hi" nil))))))
