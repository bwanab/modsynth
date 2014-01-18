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
(def points (atom {}))
(def connections (atom []))
(def busses (atom {}))

(s/svolume 0.0)

(defn all-bus-monitors []
  (s/bus-monitor-group @busses))

(defn print-monitors [bmg]
  (doseq [k (keys bmg)] (println k ":" (deref (get bmg k)))))

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
  (let [widget (get-in w [:node :widget])
        b (:point w)
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
       (let [w1 (get @points id1)
             w2 (get @points id2)
             [x1 y1 x2 y2] (concat (getXY w1) (getXY w2))]
         ;(println x1 y1 x2 y2)
         (.drawLine g x1 y1 x2 y2)))
     (.setColor g cur-color)
    ))

(defn get-synth-controls [s]
  (filter #(not (contains? #{"obus" "ibus"} %)) (map :name (:params s))))

(defn make-io [io t node-id]
  (let [m (when-let [b (get io t)]
            (button :text b :class t :id (str node-id "-" b)))
        r (when (= t :input)
            (for [c (get-synth-controls (:synth-type io))]
              (button :text c :class t :id (str node-id "-" c))))]
    (if m
      (cons m r)
      (if r r []))))

(defn get-point-name [point]
  (let [n (:node point)
        p (:point point)]
    (str (:name n) "-" (text p))))

(defn get-synths [in out]
  (let [win (:node in)
        wout (:node out)
        in-name (get-point-name in)
        out-name (get-point-name out)
        ctl-name (text (:point out))
        out-bus (if (contains? (set (get-synth-controls (:synth-type wout))) ctl-name)
                  (keyword ctl-name)
                  :ibus)]
    ;;(println "out-name = " out-name " controls = " (get-synth-controls (:synth-type wout)) "out-bus = " out-bus)
    [(:synth win) (:synth wout) (:out-type win) out-bus (str in-name " -> " out-name)]))

(defn connect-points [lpoint wpoint]
  (let [[n1 n2 out-type ctl bus-name] (if (= (button-type (:point lpoint)) :output)
                                        (get-synths lpoint wpoint)
                                        (get-synths wpoint lpoint))]
    (s/connect-points n1 n2 out-type ctl bus-name)))

(defn get-params [stype name]
  (first (filter #(= (:name %) name) (:params stype))))

(defn connect-manual [from to]
  (let [name (text (:point to))
        t (keyword name)
        from-synth (get-in from [:node :synth])  ;; from-synth is currently a slider widget
        to-synth (get-in to [:node :synth])
        p (get-params (get-in to [:node :synth-type]) name)
        mn (:min p)
        mx (:max p)
        df (:default p)]
    (println "from=" from)
    (println "to=" to)
    (println "name=" name)
    (println "from-synth=" from-synth)
    (println "to-synth=" to-synth)
    (config! from-synth :min mn :max mx :value df :paint-labels? true :paint-ticks? true)
    (listen from-synth :change (fn [e] (s/sctl to-synth t (value from-synth))))))

(defn make-synth [s]
  (let [synth (s)]
    (doseq [p (get-synth-controls s)]
      (let [c (s/const)
            val (:def p)]
        (when val (do
                    (s/connect-points c s :control (:keyword p))
                    (s/sctl c :val val)))))
    synth))


"
Each element on the screen is a node. Each node represents a visual widget and a synth or a manual controller like a slider. Every node
is stored in a map nodes keyed by its id.

Each connection point is a button of a node that represents some parameter of the node's synth.
Connections are references to two connection points
"

(defn add-node [& ps]
  (let [io (apply hash-map ps)
        id (:name io)
        kw (keyword id)
        ins  (make-io io :input id)
        outs  (make-io io :output id)
        node (assoc io :widget (doto (border-panel :id id
                                           :border (line-border :top 1 :color "#AAFFFF")
                                           :north (label :text id :background "#AAFFFF" :h-text-position :center)
                                           :center (if (= nil (:cent io)) (label id) (:cent io))
                                           :east (grid-panel :rows (count outs) :columns 1 :items outs)
                                           :west (grid-panel :rows (count ins) :columns 1 :items ins)
                                           )
                                   (config! :bounds :preferred)
                                   movable)
                    :id kw :inputs ins :outputs outs)]
    (println "add node " id)
    (swap! nodes assoc kw node)
    (doseq [b (concat ins outs)]
      (let [wpoint {:point b :node node}
            wid (config b :id)]
        (swap! points assoc wid wpoint)
        (listen b :action
                (fn [e]
                  (if-let [lid (:last-point @s-panel)]
                    (let [lpoint (get @points lid)]
                      (println "connecting points " lid wid)
                      (when (not= lid wid) ; don't connect inputs to outputs of same node
                        (cond (= :manual (get-in lpoint [:node :out-type])) (connect-manual lpoint wpoint)
                              (= :manual (get-in wpoint [:node :out-type])) (connect-manual wpoint lpoint)
                              :else
                              (let [c (connect-points lpoint wpoint)]
                                (swap! busses assoc (:name c) c)))
                        (swap! connections conj [lid wid])
                        (swap! s-panel assoc :last-point  nil)))
                    (swap! s-panel assoc :last-point wid))))))
    (config! (:panel @s-panel)
             :items (conj (config (:panel @s-panel) :items)
                          (:widget node)))
    node))

(defn get-id [t e]
  (let [id (if (number? e) e (swap! next-id inc))]
    (str t ":" id)))

(defn- osc [name synth-type]
  (add-node :name name :synth (make-synth synth-type) :input "freq" :output "sig" :out-type :audio :synth-type synth-type ))

(defn saw-osc [e]
  (let [id (get-id "saw-osc" e)]
    (osc id s/saw-osc)))

(defn square-osc [e]
  (let [id (get-id "square-osc" e)]
    (add-node :name id :synth (make-synth s/square-osc) :input "freq" :output "sig" :out-type :audio :synth-type s/square-osc)))

(defn sin-osc [e]
  (let [id (get-id "sin-osc" e)]
    (osc id s/s_sin-osc)))

(defn lp-filt [e]
  (let [id (get-id "lp-filt" e)]
    (add-node :name id :synth (make-synth s/lp-filt) :input "in" :output "out" :out-type :audio :synth-type s/lp-filt )))

(defn hp-filt [e]
  (let [id (get-id "hp-filt" e)]
    (add-node :name id :synth (make-synth s/hp-filt) :input "in" :output "out" :out-type :audio :synth-type s/hp-filt) ))

(defn bp-filt [e]
  (let [id (get-id "bp-filt" e)]
    (add-node :name id :synth (make-synth s/bp-filt) :input "in" :output "out" :out-type :audio :synth-type s/bp-filt) ))

(defn moog-filt [e]
  (let [id (get-id "moog-filt" e)]
    (add-node :name id :synth (make-synth s/moog-filt) :input "in" :output "out" :out-type :audio :synth-type s/moog-filt) ))

(defn freeverb [e]
  (let [id (get-id "freeverb" e)]
    (add-node :name id :synth (make-synth s/freeverb) :input "in" :output "out" :out-type :audio :synth-type s/freeverb )))

(defn echo [e]
  (let [id (get-id "echo" e)]
    (add-node :name id :synth (make-synth s/echo) :input "in" :output "out" :out-type :audio :synth-type s/echo) ))

(defn amp [e]
  (let [id (get-id "amp" e)]
    (add-node :name id :synth (make-synth s/amp) :input "in" :output "out" :out-type :audio :synth-type s/amp) ))

(defn sin-vco [e]
  (let [id (get-id "sin-vco" e)
        synth (make-synth s/sin-vco)]
    (add-node :name id :synth synth :input "freq" :output "out" :out-type :control :synth-type s/sin-vco)))

(defn note-in [e]
  (let [id (get-id "note-in" e)
        synth (make-synth s/midi-in)]
    (add-node :name id :synth synth :input "note" :output "freq" :out-type :control :synth-type s/midi-in)))

(defn audio-out [e]
  (let [id (get-id "audio-out" e)
        synth (make-synth s/audio-out)]
    (add-node :name id :synth synth :input "in" :output "out" :out-type :audio :synth-type s/audio-out)))

(defn const [e]
  (let [id (get-id "const" e)
        synth (s/const)
        t (text :text "     " :columns 5)]
    (config! t :listen [:action (fn [e] (s/sctl synth :ibus (read-string (text t))))])
    (add-node :name id :synth synth :output "val" :out-type :control :synth-type s/const
              :cent t)))

(defn midi-in [e]
  (let [id (get-id "midi-in" e)
        synth (s/midi-in)]
    (add-node :name id :synth synth :output "freq" :out-type :control
                :cent (text :text "" :columns 1
                            :listen [:key-pressed (fn [e]
                                                    (println (.getKeyCode e))
                                                    (s/sctl synth :note (.getKeyCode e)))]))))

(defn piano-in [e]
  (let [id (get-id "piano-in" e)
        synth (s/midi-in)
        p (piano (fn [k] (s/sctl synth :note k))
                 (fn [k] (s/sctl synth :note -1000)))]
    (show! p)
    (add-node :name id :synth synth :output "freq" :out-type :control)))

(defn slider-ctl [e]
  (let [id (get-id "slider-ctl" e)
        s (slider :value 0 :min 0 :max 100 :orientation :vertical)]
    (add-node :name id :synth s :output "val" :out-type :manual :cent s)))

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
    (swap! s-panel assoc :panel p :last-point nil :master-vol 0.3)
    (frame
     :menubar (menubar :items [(menu :text "File"
                                     :items [(action :handler sound-on :name "Sound On")
                                             (action :handler sound-off :name "Sound Off")
                                             (action :handler dispose! :name "Exit")])
                               (menu :text "New Control"
                                     :items [(action :handler saw-osc :name "Saw Osc")
                                             (action :handler square-osc :name "Square Osc")
                                             (action :handler sin-osc :name "Sin Osc")
                                             (action :handler sin-vco :name "Sin VCO")
                                             (action :handler const :name "Const")
                                             (action :handler midi-in :name "Midi In")
                                             (action :handler note-in :name "Note In")
                                             (action :handler piano-in :name "Piano In")
                                             (action :handler lp-filt :name "LP Filt")
                                             (action :handler hp-filt :name "HP Filt")
                                             (action :handler moog-filt :name "Moog Filt")
                                             (action :handler freeverb :name "Freeverb")
                                             (action :handler echo :name "Echo")
                                             (action :handler amp :name "Amp")
                                             (action :handler slider-ctl :name "Slider")
                                             (action :handler audio-out :name "Audio out")
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
;;(connect-points {:node (get @synths (name (id-of m))) :type (button-type b1) :out-type :control}  ;connect the synths
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
           {:x (.getX l) :y (.getY l) :w (config w :id)}))
       (vals @nodes)))

(defn dump-synth [s]
  (symbol (second (first s))))

(defn dump-connections []
  @connections)

(defn dump-all []
  {:nodes (symbol (str "'" (to-string (dump-nodes))))
   :connections (symbol (str "'" (to-string (dump-connections))))
   :master-vol (:master-vol @s-panel)})

(defn make-node [ntype id x y]
  (let [s (str "(" ntype " " id ")")
        m (load-string s)]
    (move! (:widget m) :by [x y])
    m))

(defn get-node-name [node-name]
  (let [s (str/split node-name #":")
        n (str/split (second s) #"-")]
    (str (first s) ":" (first n))))

(defn ms-reset! []
  (sound-off 0)
  (reset! connections [])
  (reset! points {})
  (reset! nodes {})
  (reset! busses {})
  (reset! next-id 0)
  )

(defn restore [n]
  (ms-reset!)
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
            k1 (keyword (get-node-name name1))
            k2 (keyword (get-node-name name2))
            m1 (get m k1)
            m2 (get m k2)
            b1 (select f [(keyword (str "#" name1))])
            b2 (select f [(keyword (str "#" name2))])
            node1 {:point b1 :node m1}
            node2 {:point b2 :node m2}]
        (cond (= :manual (get-in node1 [:node :out-type])) (connect-manual node1 node2)
              (= :manual (get-in node2 [:node :out-type])) (connect-manual node2 node1)
              :else
              (connect-points node1 node2))
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
;;     (connect-points {:node (get @synths (name (id-of m))) :type (button-type b1) :out-type :control}  ;connect the synths
;;                    {:node (get @synths (name (id-of o))) :type (button-type b2) :out-type :audio})
;;     (swap! connections conj [mnode onode])))
