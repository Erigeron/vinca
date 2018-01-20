
(ns app.comp.container
  (:require [hsl.core :refer [hsl]]
            [respo-ui.core :as ui]
            [respo.macros
             :refer
             [defcomp cursor-> action-> mutation-> <> div button textarea span]]
            [verbosely.core :refer [verbosely!]]
            [respo.comp.space :refer [=<]]
            [reel.comp.reel :refer [comp-reel]]
            [app.interpreter :refer [load-program]]
            [cljs.reader :refer [read-string]]))

(defcomp
 comp-container
 (reel)
 (let [store (:store reel), states (:states store)]
   (div
    {:style (merge ui/global ui/row)}
    (textarea
     {:value (:content store),
      :placeholder "Content",
      :style (merge ui/textarea {:width 640, :height 320, :font-family ui/font-code}),
      :on-input (action-> :content (:value %e))})
    (=< "8px" nil)
    (div
     {}
     (button
      {:style ui/button,
       :inner-text (str "run"),
       :on-click (fn [e d! m!] (load-program (read-string (:content store))))}))
    (cursor-> :reel comp-reel states reel {}))))
