;  Copyright (c) Bill Allen, 2013. All rights reserved.

;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this
;   distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns modsynth.core
  (:use [seesaw core border behave graphics]
        [modsynth.piano])
  (:require [modsynth.synths :as s])
  (:import [javax.swing SwingUtilities]
           [java.awt Color]))

(native!)
(def s-panel (atom {}))
(def next-id (atom 0))
(def nodes (atom []))
(def connections (atom []))
(def synths (atom {}))

(s/svolume 0.0)

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
(defrecord IONode [w b ot st])   ;; widget, button, output-type (:control, :audio), synth type (e.g. lp-filt)

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
        cur-color (.getColor g)]
    (doseq [x (range 0 w 10)]
      (.drawLine g x 0 x h))
    (doseq [y (range 0 h 10)]
      (.drawLine g 0 y w y))
     (.setColor g (Color/decode "#FFFFaa"))
     (doseq [[w1 w2] @connections]
       (let [[x1 y1 x2 y2] (concat (getXY w1) (getXY w2))]
         ;(println x1 y1 x2 y2)
         (.drawLine g x1 y1 x2 y2)))
     (.setColor g cur-color)
    ))

(defn make-io [io t]
  (let [v (get io t)]
    (for [b v] (button :text b :class t))))

(defn connect-nodes [lnode wnode]
  (let [[n1 n2 out-type]
        (if (= (:type lnode) :output)
          [(:node lnode) (:node wnode) (:otype lnode)]
          [(:node wnode) (:node lnode) (:otype lnode)])]
    (println n1 n2 out-type)
    (s/connect-nodes n1 n2 out-type)))

(defn get-params [stype name]
  (first (filter #(= (:name %) name) (:params stype))))

(defn connect-manual [w synth b stype]
  (let [name (text b)
        t (keyword name)
        p (get-params stype name)
        mn (:min p)
        mx (:max p)
        df (:default p)]
    (println p)
    (config! w :min mn :max mx :value df :paint-labels? true :paint-ticks? true)
    (listen w :change (fn [e] (s/sctl synth t (value w))))))

(defn add-widget [t io cent]
  (let [id t
        kw (keyword id)
        ins  (make-io io :input)
        outs  (make-io io :output)
        out-type (:otype io)
        stype (:stype io)
        widget (doto (border-panel :id id
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
            wnode (IONode. widget b out-type stype)]
        (listen b :action
                (fn [e]
                  (if-let [lnode (:last-node @s-panel)]
                    (when (not= (:w lnode) (:w wnode)) ; don't connect inputs to outputs of same widget
                      (let [lsynth (get @synths (name (config (:w lnode) :id)))
                            wsynth (get @synths (name (config (:w wnode) :id)))]
                        ; TODO: not taking into account the button yet. Just assuming that every widget has 1 in 1 out
                        (cond (= :manual (:ot lnode)) (connect-manual lsynth wsynth (:b wnode) (:st wnode))
                              (= :manual (:ot wnode)) (connect-manual wsynth lsynth (:b lnode) (:st lnode))
                              :else
                              (connect-nodes {:node lsynth :type (button-type (:b lnode)) :otype (:ot lnode)}
                                             {:node wsynth :type (button-type (:b wnode)) :otype (:ot wnode)}))
                        (swap! connections conj [lnode wnode])
                        (swap! s-panel assoc :last-node  nil)))
                    (swap! s-panel assoc :last-node wnode))))))
    (swap! nodes conj widget)
    (config! (:panel @s-panel)
             :items (conj (config (:panel @s-panel) :items)
                          widget))))

(defn get-id [t]
  (str t (swap! next-id inc)))

(defn- osc [e type]
  (add-widget type {:input ["freq"] :output ["sig"] :otype :audio} (label type)))

(defn saw-osc [e]
  (let [id (get-id "saw-osc")]
    (swap! synths assoc id (s/saw-osc))
    (osc e id)))

(defn square-osc [e]
  (let [id (get-id "square-osc")]
    (swap! synths assoc id (s/square-osc))
    (add-widget id
                {:input ["freq" "width"] :output ["sig"] :otype :audio :stype s/square-osc}
                (label "sq"))))

(defn sin-osc [e]
  (let [id (get-id "sin-osc")]
    (swap! synths assoc id (s/s_sin-osc))
    (osc e id)))

(defn lp-filt [e]
  (let [id (get-id "lp-filt")]
    (swap! synths assoc id (s/lp-filt))
    (add-widget id {:input ["in" "cutoff"] :output ["out"] :otype :audio :stype s/lp-filt} (label "lpf"))))

(defn hp-filt [e]
  (let [id (get-id "hp-filt")]
    (swap! synths assoc id (s/hp-filt))
    (add-widget id {:input ["in" "cutoff"] :output ["out"] :otype :audio :stype s/hp-filt} (label "hpf"))))

(defn bp-filt [e]
  (let [id (get-id "bp-filt")]
    (swap! synths assoc id (s/bp-filt))
    (add-widget id {:input ["in" "freq" "q"] :output ["out"] :otype :audio :stype s/bp-filt} (label "bpf"))))

(defn moog-filt [e]
  (let [id (get-id "moog-filt")]
    (swap! synths assoc id (s/moog-filt))
    (add-widget id {:input ["in" "cutoff" "lpf-res"] :output ["out"] :otype :audio :stype s/moog-filt} (label "moogf"))))

(defn freeverb [e]
  (let [id (get-id "freeverb")]
    (swap! synths assoc id (s/freeverb))
    (add-widget id {:input ["in" "wet-dry" "room-size" "dampening"] :output ["out"] :otype :audio :stype s/freeverb} (label "freeverb"))))

(defn echo [e]
  (let [id (get-id "echo")]
    (swap! synths assoc id (s/echo))
    (add-widget id {:input ["in" "max-delay" "delay-time" "decay-time"] :output ["out"] :otype :audio :stype s/echo} (label "echo"))))

(defn amp [e]
  (let [id (get-id "amp")]
    (swap! synths assoc id (s/amp))
    (add-widget id {:input ["in" "gain"] :output ["out"] :otype :audio :stype s/amp} (label "amp"))))

(defn midi-in [e]
  (let [id (get-id "midi-in")]
    (swap! synths assoc id (s/midi-in))
    (add-widget id
                {:output ["freq"] :otype :control}
                (text :text ""
                      :listen [:key-pressed (fn [e]
                                              (println (.getKeyCode e))
                                              (s/sctl (get @synths id) :note (.getKeyCode e))
                                              )]))))

(defn piano-in [e]
  (let [id (get-id "piano-in")
        synth (s/midi-in)
        p (piano (fn [k] (s/sctl synth :note k))
                 (fn [k] (s/sctl synth :note -1000)))]
    (swap! synths assoc id synth)
    (show! p)
    (add-widget id
                {:output ["freq"] :otype :control}
                (label ""))))

(defn slider-ctl [e]
  (let [id (get-id "slider")
        s (slider :value 0 :min 0 :max 100 :orientation :vertical)]
    (swap! synths assoc id s)
    (add-widget id
                {:output ["out"] :otype :manual}
                s)))

(defn sound-on [e]
  (s/svolume (:master-vol @s-panel)))
(defn sound-off [e]
  (s/svolume 0.0))


(defn make-panel []
  (xyz-panel
    :paint draw-grid
    :id :xyz
    :background "#222222"
    :items []
    ))

(defn fr []
  (let [p (make-panel)]
    (swap! s-panel assoc :panel p :last-node nil :master-vol 0.3)
    (frame
     :menubar (menubar :items [(menu :text "File"
                                     :items [(action :handler sound-on :name "Sound On")
                                             (action :handler sound-off :name "Sound Off")
                                             (action :handler dispose! :name "Exit")])
                               (menu :text "New Control"
                                     :items [(action :handler saw-osc :name "Saw Osc")
                                             (action :handler square-osc :name "Square Osc")
                                             (action :handler sin-osc :name "Sin Osc")
                                             (action :handler midi-in :name "Midi In")
                                             (action :handler piano-in :name "Piano In")
                                             (action :handler lp-filt :name "LP Filt")
                                             (action :handler hp-filt :name "LP Filt")
                                             (action :handler moog-filt :name "Moog Filt")
                                             (action :handler freeverb :name "Freeverb")
                                             (action :handler echo :name "Echo")
                                             (action :handler amp :name "Amp")
                                             (action :handler slider-ctl :name "Slider")
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
