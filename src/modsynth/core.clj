;  Copyright (c) Bill Allen, 2013. All rights reserved.

;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this
;   distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns modsynth.core
  (:use [seesaw core border behave graphics])
  (:require [overtone.live :as o])
  (:import [javax.swing SwingUtilities]
           [java.awt Color]))

(native!)
(def next-id (atom 0))
(def s-panel (atom {}))

; Put in some basic support for moving w around using behave/when-mouse-dragged.
(defn movable [w]
  (let [start-point (java.awt.Point.)]
    (when-mouse-dragged w
      ; When the mouse is pressed, move the widget to the front of the z order
      :start (fn [e]
                (move! e :to-front)
                (.setLocation start-point (.getPoint e)))
      ; When the mouse is dragged move the widget
      ; Unfortunately, the delta passed to this function doesn't work correctly
      ; if the widget is moved during the drag. So, the move is calculated
      ; manually.
      :drag (fn [e _]
              (let [p (.getPoint e)]
                (move! e :by [(- (.x p) (.x start-point))
                              (- (.y p) (.y start-point))])))))
  w)


;;(defrecord IONode [w b wa ha])   ;; widget, button, width adjustment, height adjustment
;; wa and ha aren't known when IONode is created TODO: solve this so we don't compute this over and over
(defrecord IONode [w b])   ;; widget, button

(defn button-type [b]
  (keyword (first (config b :class))))

(defn get-io-width-adjustment [w]
  (if (= :output (button-type (:b w)))
    (int (.getWidth (:w w)) )
    0))

(defn get-io-height-adjustment [b]
  (int (* 1 (.getHeight (config b :size)))))

(defn getXY
  "return int values for x and y for a widget"
  [w]
  (let [widget (:w w)
        b (:b w)
        ;; ew (:wa w)
        ;; eh (:ha w)
        ew (get-io-width-adjustment w)
        eh (get-io-height-adjustment b)
        wp (config widget :location)
        bp (config b :location)]
    [(int (+ ew (.getX wp) (.getX bp)))
     (int (+ eh (.getY wp) (.getY bp)))])
  )

(defn draw-grid [c g]
  (let [w (width c) h (height c)
        cur-color (.getColor g)
        cables (:cables @s-panel)]
    (doseq [x (range 0 w 10)]
      (.drawLine g x 0 x h))
    (doseq [y (range 0 h 10)]
      (.drawLine g 0 y w y))
     (.setColor g (Color/decode "#FFFFaa"))
     (doseq [cable cables]
       (let [[w1 w2] cable
             [x1 y1 x2 y2] (concat (getXY w1) (getXY w2))]
         ;(println x1 y1 x2 y2)
         (.drawLine g x1 y1 x2 y2)))
     (.setColor g cur-color)
    ))

(defn make-io [io t]
  (let [v (get io t)]
    (for [b v] (button :text b :class t))))

(defn add-widget [t io cent]
  (let [id (str t (swap! next-id inc))
        kw (keyword id)
        ins  (make-io io :input)
        outs  (make-io io :output)
        widget (doto (border-panel
                      :border (line-border :top 1 :color "#AAFFFF")
                      :north (label :text id :background "#AAFFFF" :h-text-position :center)
                      :center cent
                      :east (grid-panel :rows (count outs) :columns 1 :items outs)
                      :west (grid-panel :rows (count ins) :columns 1 :items ins)
                      )
                 (config! :bounds :preferred)
                 movable)
        ]
    (doseq [b (concat ins outs)]
      (let [
            ;;wa (get-io-width-adjustment b)
            ;;ha (get-io-height-adjustment b)
            wnode (IONode. widget b)]
        (listen b :action
                (fn [e]
                  (if-let [lnode (:last-widget @s-panel)]
                    (when (not= (:w lnode) (:w wnode)) ; don't connect inputs to outputs of same widget
                      (do
                        (swap! s-panel (fn [m k v] (assoc m k (cons v (get m k)))) :cables [lnode wnode])
                        (swap! s-panel assoc :last-widget nil)))
                    (swap! s-panel assoc :last-widget wnode))))))
    (swap! s-panel (fn [m k v] (assoc m k (cons v (get m k)))) :nodes widget)
    (config! (:panel @s-panel)
             :items (conj (config (:panel @s-panel) :items)
                          widget))))

(defn osc [e type]
  (add-widget type {:input ["freq"] :output ["sig"]} (label type)))

(defn saw-osc [e]
  (osc e "saw"))

(defn square-osc [e]
  (add-widget "square"
              {:input ["freq" "width"] :output ["sig"]}
              (label "sq")))

(defn sin-osc [e]
  (osc e "sin"))


(defmacro mod-defsynth [name p body] (let [sym-name (symbol (eval name))] `(o/defsynth ~sym-name ~p ~body)))
(defmacro mod-ctl [name t val] (let [sym-name (symbol (eval name))] `(o/ctl ~sym-name ~t ~val)))


(defn midi-in [e]
  (let [id @next-id]
    (mod-defsynth (str "midi-in" 1)
                  [obus 0
                   note {:default 60 :min 0 :max 120 :step 1}]
                  (let [freq (o/midicps note)]
                    (o/out:kr obus freq)))
    (add-widget "midi-in"
                {:output ["freq"]}
                (text :text ""
                      :listen [:key-pressed (fn [e]
                                              (println (.getKeyCode e))
                                              (mod-ctl (str "midi-in" 1) :note (.getKeyCode e))
                                              )]))))

(defn make-panel []
  (xyz-panel
    :paint draw-grid
    :id :xyz
    :background "#222222"
    :items []
    ))

(defn fr []
  (let [p (make-panel)]
    (swap! s-panel assoc :panel p :cables [])
    (frame
     :menubar (menubar :items [(menu :text "File"
                                     :items [(action :handler dispose! :name "Exit")])
                               (menu :text "New Control"
                                     :items [(action :handler saw-osc :name "Saw Osc")
                                             (action :handler square-osc :name "Square Osc")
                                             (action :handler sin-osc :name "Sin Osc")
                                             (action :handler midi-in :name "Midi In")
                                             ])])
     :title   "Overtone Modular Synth"
     :content (border-panel
               :vgap 5
               :center p)
     :size    [600 :by 600])))

(defn bugger-what!
  "for some reason, makes the frame show full size"
  [f]
  (if (= (java.awt.Dimension.) (.getSize f))
    (pack! f)
    f))

(defn -main [& args]
  (let [f (invoke-now (fr))]
    (swap! s-panel assoc :frame fr)
    (config! f :on-close :dispose)
    (-> f bugger-what! show!)))
