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
        [modsynth.piano]
        [clojure.pprint :only [write]])
  (:require [modsynth.synths :as s]
            [clojure.string :as str])
  (:import [javax.swing SwingUtilities]
           [java.awt Color]))

(native!)
(def s-panel (atom {}))
(def next-id (atom 0))
(def nodes (atom {}))
(def connection-points (atom {}))
(def connections (atom []))

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


(defn button-type [b]
  (keyword (first (config b :class))))

(defn get-io-width-adjustment [w b]
  (if (= :output (button-type b))
    (int (.getWidth w) )
    0))

(defn get-io-height-adjustment [b]
  (int (* 1 (.getHeight (config b :size)))))

(defn getXY
  "return int values for x and y for a widget"
  [w]
  (let [widget (get-in w [:widget :widget])
        b (:con-point w)
        ;; ew (:wa w)
        ;; eh (:ha w)
        ew (get-io-width-adjustment widget b)
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
     (doseq [[id1 id2] @connections]
       (let [w1 (get @connection-points id1)
             w2 (get @connection-points id2)
             [x1 y1 x2 y2] (concat (getXY w1) (getXY w2))]
         ;(println x1 y1 x2 y2)
         (.drawLine g x1 y1 x2 y2)))
     (.setColor g cur-color)
    ))

(defn make-io [io t widget-id]
  (let [v (get io t)]
    (for [b v] (button :text b :class t :id (str widget-id "-" b)))))

(defn get-synths [in out]
  (let [win (:widget in)
        wout (:widget out)]
    [(:synth win) (:synth wout) (:out-type win)]))

(defn connect-nodes [lnode wnode]
  (let [[n1 n2 out-type]
        (if (= (button-type (:con-point lnode)) :output)
          (get-synths lnode wnode)
          (get-synths wnode lnode))]
    (println lnode wnode)
    (println n1 n2 out-type)
    (s/connect-nodes n1 n2 out-type)))

(defn get-params [stype name]
  (first (filter #(= (:name %) name) (:params stype))))

(defn connect-manual [from to]
  (let [name (text (:con-point to))
        t (keyword name)
        from-synth (get-in from [:widget :synth])  ;; from-synth is currently a slider widget
        to-synth (get-in to [:widget :synth])
        p (get-params (get-in to [:widget :synth-type]) name)
        mn (:min p)
        mx (:max p)
        df (:default p)]
    (println p)
    (config! from-synth :min mn :max mx :value df :paint-labels? true :paint-ticks? true)
    (listen from-synth :change (fn [e] (s/sctl to-synth t (value from-synth))))))

"
Each element on the screen is a widget. Each widget represents a synth or a manual controller like a slider. Every widget
is stored in a map nodes keyed by its id.

Each connection point is a button of a widget that represents some parameter of the widget's synth.
Connections are references to two connection points
"

(defn add-widget [io cent]
  (let [id (:name io)
        kw (keyword id)
        ins  (make-io io :input id)
        outs  (make-io io :output id)
        widget (assoc io :widget (doto (border-panel :id id
                                           :border (line-border :top 1 :color "#AAFFFF")
                                           :north (label :text id :background "#AAFFFF" :h-text-position :center)
                                           :center cent
                                           :east (grid-panel :rows (count outs) :columns 1 :items outs)
                                           :west (grid-panel :rows (count ins) :columns 1 :items ins)
                                           )
                                   (config! :bounds :preferred)
                                   movable)
                      :id kw :inputs ins :outputs outs)]
    (swap! nodes assoc kw widget)
    (doseq [b (concat ins outs)]
      (let [wnode {:con-point b :widget widget}
            wid (config b :id)]
        (swap! connection-points assoc wid wnode)
        (listen b :action
                (fn [e]
                  (if-let [lid (:last-node @s-panel)]
                    (let [lnode (get @connection-points lid)]
                      (when (not= (:widget lnode) (:widget wnode)) ; don't connect inputs to outputs of same widget
                        (cond (= :manual (get-in lnode [:widget :out-type])) (connect-manual lnode wnode)
                              (= :manual (get-in wnode [:widget :out-type])) (connect-manual wnode lnode)
                              :else
                              (connect-nodes lnode wnode))
                        (swap! connections conj [lid wid])
                        (swap! s-panel assoc :last-node  nil)))
                    (swap! s-panel assoc :last-node wid))))))
    (config! (:panel @s-panel)
             :items (conj (config (:panel @s-panel) :items)
                          (:widget widget)))
    widget))

(defn get-id [t e]
  (let [id (if (number? e) e (swap! next-id inc))]
    (str t ":" id)))

(defn- osc [name synth-type]
  (add-widget {:name name :synth (synth-type) :input ["freq"] :output ["sig"] :out-type :audio :synth-type synth-type} (label name)))

(defn saw-osc [e]
  (let [id (get-id "saw-osc" e)]
    (osc id s/saw-osc)))

(defn square-osc [e]
  (let [id (get-id "square-osc" e)]
    (add-widget {:name id :synth (s/square-osc) :input ["freq" "width"] :output ["sig"] :out-type :audio :synth-type s/square-osc}
                (label "sq"))))

(defn sin-osc [e]
  (let [id (get-id "sin-osc" e)]
    (osc e id s/s_sin-osc)))

(defn lp-filt [e]
  (let [id (get-id "lp-filt" e)]
    (add-widget {:name id :synth (s/lp-filt) :input ["in" "cutoff"] :output ["out"] :out-type :audio :synth-type s/lp-filt} (label "lpf"))))

(defn hp-filt [e]
  (let [id (get-id "hp-filt" e)]
    (add-widget {:name id :synth (s/hp-filt) :input ["in" "cutoff"] :output ["out"] :out-type :audio :synth-type s/hp-filt} (label "hpf"))))

(defn bp-filt [e]
  (let [id (get-id "bp-filt" e)]
    (add-widget {:name id :synth (s/bp-filt) :input ["in" "freq" "q"] :output ["out"] :out-type :audio :synth-type s/bp-filt} (label "bpf"))))

(defn moog-filt [e]
  (let [id (get-id "moog-filt" e)]
    (add-widget {:name id :synth (s/moog-filt) :input ["in" "cutoff" "lpf-res"] :output ["out"] :out-type :audio :synth-type s/moog-filt} (label "moogf"))))

(defn freeverb [e]
  (let [id (get-id "freeverb" e)]
    (add-widget {:name id :synth (s/freeverb) :input ["in" "wet-dry" "room-size" "dampening"] :output ["out"] :out-type :audio :synth-type s/freeverb} (label "freeverb"))))

(defn echo [e]
  (let [id (get-id "echo" e)]
    (add-widget {:name id :synth (s/echo) :input ["in" "max-delay" "delay-time" "decay-time"] :output ["out"] :out-type :audio :synth-type s/echo} (label "echo"))))

(defn amp [e]
  (let [id (get-id "amp" e)]
    (add-widget {:name id :synth (s/amp) :input ["in" "gain"] :output ["out"] :out-type :audio :synth-type s/amp} (label "amp"))))

(defn midi-in [e]
  (let [id (get-id "midi-in" e)
        synth (s/midi-in)]
    (add-widget {:name id :synth synth :output ["freq"] :out-type :control}
                (text :text ""
                      :listen [:key-pressed (fn [e]
                                              (println (.getKeyCode e))
                                              (s/sctl synth :note (.getKeyCode e))
                                              )]))))

(defn piano-in [e]
  (let [id (get-id "piano-in" e)
        synth (s/midi-in)
        p (piano (fn [k] (s/sctl synth :note k))
                 (fn [k] (s/sctl synth :note -1000)))]
    (show! p)
    (add-widget {:name id :synth synth :output ["freq"] :out-type :control}
                (label ""))))

(defn slider-ctl [e]
  (let [id (get-id "slider" e)
        s (slider :value 0 :min 0 :max 100 :orientation :vertical)]
    (add-widget {:name id :synth s :output ["out"] :out-type :manual}
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
    (swap! s-panel assoc :frame f)
    (config! f :on-close :dispose)
    (-> f bugger-what! show!)))

;;
;;(def f (-main))              ; create frame
;;(def m (midi-in 1))          ; add a midi-in
;;(def o (saw-osc 1))          ; add a saw-osc
;;(move! m :by [50 50])        ; move them on the frame
;;(move! o :by [100 100])
;;(def b1 (select f [:#midi-in1freq]))  ; get the two buttons we want to connect
;;(def b2 (select f [:#saw-osc2freq]))
;;(connect-nodes {:node (get @synths (name (id-of m))) :type (button-type b1) :out-type :control}  ;connect the synths
;;               {:node (get @synths (name (id-of o))) :type (button-type b2) :out-type :audio})
;;(def mnode (modsynth.core.IONode. m b1 :control s/midi-in)) ; make IONode for midi-in/freq
;;(def onode (modsynth.core.IONode. o b2 :freq s/saw-osc))    ; make IONode for saw-osc/freq
;;(swap! connections conj [mnode onode]                       ; put them in connections which causes connection to draw

(defn to-string [e]
  (write e :stream nil :pretty false))

(defn dump-nodes []
  (map (fn [e]
         (let [w (:widget e)
               l (config w :location)]
           {:x (.getX l) :y (.getY l) :w (config w :id)})) (vals @nodes)))

(defn dump-synth [s]
  (symbol (second (first s))))

(defn dump-connections []
  @connections)

(defn dump-all []
  {:nodes (symbol (str "'" (to-string (dump-nodes)))) :connections (symbol (str "'" (to-string (dump-connections)))) :master-vol (:master-vol @s-panel)})

(defn make-node [ntype id x y]
  (let [s (str "(" ntype " " id ")")
        m (load-string s)]
    (move! (:widget m) :by [x y])
    m))

(defn get-widget-name [node-name]
  (let [s (str/split node-name #":")
        n (str/split (second s) #"-")]
    (str (first s) ":" (first n))))

(defn restore [n]
  (sound-off 0)
  (reset! connections [])
  (reset! connection-points {})
  (reset! nodes {})
  (reset! next-id 0)
  (swap! s-panel assoc :master-vol (:master-vol n))
  (let [f (-main)
        m (apply hash-map
                 (flatten
                  (for [n (:nodes n)]
                    (let [s (str/split (name (:w n)) #":")
                          wname (first s)
                          wnum  (second s)
                          x (:x n)
                          y (:y n)]
                      [(:w n) (make-node wname wnum x y)]))))]
    (doseq [[n1 n2] (:connections n)]
      (let [name1 (name n1)
            name2 (name n2)
            k1 (keyword (get-widget-name name1))
            k2 (keyword (get-widget-name name2))
            m1 (get m k1)
            m2 (get m k2)
            b1 (select f [(keyword (str "#" name1))])
            b2 (select f [(keyword (str "#" name1))])
            node1 {:con-point b1 :widget m1}
            node2 {:con-point b2 :widget m2}]
        (println node1)
        (println node2)
        (connect-nodes node1 node2)
        (swap! connections conj [n1 n2]))))
    (sound-on 0))

;; (defn test-modsynth []
;;   (let [f (-main)
;;         m (make-node "midi-in" 50 50)
;;         o (make-node "saw-osc" 100 100)
;;         b1 (select f [(keyword (str "#" (name (id-of m)) "-" "freq"))])
;;         b2 (select f [(keyword (str "#" (name (id-of o)) "-" "freq"))])
;;         mnode (modsynth.core.IONode. m b1 :control s/midi-in)
;;         onode (modsynth.core.IONode. o b2 :freq s/saw-osc)]
;;     (connect-nodes {:node (get @synths (name (id-of m))) :type (button-type b1) :out-type :control}  ;connect the synths
;;                    {:node (get @synths (name (id-of o))) :type (button-type b2) :out-type :audio})
;;     (swap! connections conj [mnode onode])))
