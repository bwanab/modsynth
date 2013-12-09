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

(defn make-line
  [x1 y1 x2 y2]
  (doto
      (label
       :border 5
       :text "dork"
       :location [(rand-int 300) (rand-int 300)]
       :paint {
               :before (fn [c g]
                         (draw g (line 1 1 (width c) (height c))
                               (style :foreground "#FFFFaa"
                                      :backgroun "#aaFFFF"
                                      :stroke 2)))})
    (config! :bounds :preferred)))

(defn make-label
  [text]
  (doto
    ; Instead of a boring label, make the label rounded with
    ; some custom drawing. Use the before paint hook to draw
    ; under the label's text.
    (label
      :border   5
      :text     text
      :location [(rand-int 300) (rand-int 300)]
      :paint {
        :before (fn [c g]
                  (draw g (rounded-rect 3 3 (- (width c) 6) (- (height c) 6) 9)
                          (style :foreground "#FFFFaa"
                                  :background "#aaFFFF"
                                  :stroke 2)))})
    ; Set the bounds to its preferred size. Note that this has to be
    ; done after the label is fully constructed.
    (config! :bounds :preferred)))


;;(defrecord IONode [w b wa ha])   ;; widget, button, width adjustment, height adjustment
;; wa and ha aren't known when IONode is created TODO: solve this so we don't compute this over and over
(defrecord IONode [w b])   ;; widget, button

(defn button-type [b]
  (keyword (first (config b :class))))

(defn get-io-width-adjustment [b]
  (if (= :output (button-type b))
    (int (.getWidth (config b :size)) )
    0))

(defn get-io-height-adjustment [b]
  (int (* 1.5 (.getHeight (config b :size)))))

(defn getXY
  "return int values for x and y for a widget"
  [w]
  (let [widget (:w w)
        b (:b w)
        ;; ew (:wa w)
        ;; eh (:ha w)
        ew (get-io-width-adjustment b)
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

(defn make-io [io]
  (let [nin (count (:inputs io))
        nout (count (:outputs io))
        ncols (max nin nout)
        items (interleave
               (concat ["Inputs"]
                       (for [b (:inputs io)] (button :text b :class :input))
                       (for [i (range nin ncols)] (label "")))
               (concat ["Outputs"]
                       (for [b (:outputs io)] (button :text b :class :output))
                       (for [i (range nout ncols)] (label ""))))]
    ;(println :nin nin :nout nout :ncols ncols)
    ;(println items)
    (grid-panel :rows  (inc ncols)
                :columns 2
                :items items
                )))

(defn add-widget [t io]
  (let [id (str t (swap! next-id inc))
        kw (keyword id)
        bs  (make-io io)
        widget (doto (border-panel
                      :border (line-border :top 15 :color "#AAFFFF")
                      :north (label id)
                      :center bs
                      )
                 (config! :bounds :preferred)
                 movable)
        ]
    (doseq [b (config bs :items)]
      (when (contains? #{:input :output} (button-type b))
        (let [
              ;;wa (get-io-width-adjustment b)
              ;;ha (get-io-height-adjustment b)
              wnode (IONode. widget b)]
          (listen b :action (fn [e]
                              (if-let [lnode (:last-widget @s-panel)]
                                (do
                                  (swap! s-panel (fn [m k v] (assoc m k (cons v (get m k)))) :cables [lnode wnode])
                                  (swap! s-panel assoc :last-widget nil))
                                (swap! s-panel assoc :last-widget wnode)))))))

    (swap! s-panel (fn [m k v] (assoc m k (cons v (get m k)))) :nodes widget)
    (config! (:panel @s-panel)
             :items (conj (config (:panel @s-panel) :items)
                          widget))))

(defn osc [e type]
  (add-widget type {:inputs ["freq"] :outputs ["sig"]}))

(defn saw-osc [e]
  (osc e "saw"))

(defn square-osc [e]
  (add-widget "square" {:inputs ["freq" "width"] :outputs ["sig"]}))

(defn sin-osc [e]
  (osc e "sin"))

(defn midi-in [e]
  (add-widget "midi-in" {:outputs ["freq"]}))

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
