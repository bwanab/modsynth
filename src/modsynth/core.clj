;  Copyright (c) Bill Allen, 2013. All rights reserved.

;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this
;   distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns modsynth.core
  (:use [seesaw core border behave graphics color]
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

(defn make-io
  [io t node-id otype]
  ;;(println t otype)
  (if (= otype :split) ; special case code for splitter
    (if (= t :input)
      [(button :text "in" :class t :id (str node-id "-in"))]
      (for [c (get-synth-controls (:synth-type io))]
        (button :text c :class t :id (str node-id "-" c))))
    (let [m (when-let [b (get io t)]
              (button :text b :class t :id (str node-id "-" b)))
          r (when (or (= t :input))
              (for [c (get-synth-controls (:synth-type io))]
                (button :text c :class t :id (str node-id "-" c))))]
      (if m
        (cons m r)
        (if r r [])))))

(defn get-point-name [point]
  (let [n (:node point)
        p (:point point)]
    (str (:name n) "-" (text p))))

(defn get-synths [in out]
  (let [win (:node in)
        wout (:node out)
        in-name (get-point-name in)
        in-ctl-name (text (:point in))
        out-name (get-point-name out)
        ctl-name (text (:point out))
        out-bus (if (contains? (set (get-synth-controls (:synth-type wout))) ctl-name)
                  (keyword ctl-name)
                  :ibus)
        in-bus (if (contains? (set (get-synth-controls (:synth-type win))) in-ctl-name)
                 (keyword in-ctl-name)
                 :obus)]
    [(:synth win) (:synth wout) (:out-type win) out-bus (str in-name " -> " out-name) in-bus]))

(defn connect-points [lpoint wpoint]
  (let [[n1 n2 out-type ctl bus-name in-ctl] (if (= (button-type (:point lpoint)) :output)
                                               (get-synths lpoint wpoint)
                                               (get-synths wpoint lpoint))]
    (s/connect-points n1 n2 out-type ctl bus-name in-ctl)))

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
    ;; (println "from=" from)
    ;; (println "to=" to)
    ;; (println "name=" name)
    ;; (println "from-synth=" from-synth)
    ;; (println "to-synth=" to-synth)
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
    {:synth synth}))

(defn get-node-name [node-name]
  (let [nname (if (keyword? node-name) (name node-name) node-name)
        s (str/split nname #":")
        n (str/split (second s) #"-")]
    (str (first s) ":" (first n))))

(defn connected?
  [lid wid]
  (not (empty? (filter #(= % [lid wid]) @connections))))

(defn make-connection
  [lid lpoint wid wpoint]
  ;;(println "lid = " lid " wid = " wid)
  (cond ;; if the two points are connected, disconnect them.
   (connected? lid wid) (reset! connections (filter #(not= % [lid wid]) @connections))
   (connected? wid lid) (reset! connections (filter #(not= % [wid lid]) @connections))
   :else (let [lpn (get-node-name lid)
               wpn (get-node-name wid)]
           (when (not= lpn wpn) ; don't connect points of same node
            (if (:run-mode @s-panel)
              (cond (= :manual (get-in lpoint [:node :out-type])) (connect-manual lpoint wpoint)
                    (= :manual (get-in wpoint [:node :out-type])) (connect-manual wpoint lpoint)
                    :else
                    (let [c (connect-points lpoint wpoint)]
                      (swap! busses assoc (:name c) c))))
            (swap! connections conj [lid wid]))))
  (swap! s-panel assoc :last-point  nil))

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
        otype (:output io)
        ins  (make-io io :input id otype)
        outs  (make-io io :output id otype)
        node (assoc io :widget (doto (border-panel :id id
                                           :border (line-border :top 1 :color "#AAFFFF")
                                           :north (label :text id :background "#AAFFFF" :h-text-position :center)
                                           :center (if (= nil (:cent io)) (label "") (:cent io))
                                           :east (grid-panel :rows (count outs) :columns 1 :items outs)
                                           :west (grid-panel :rows (count ins) :columns 1 :items ins)
                                           )
                                   (config! :bounds :preferred)
                                   movable)
                    :id kw :inputs ins :outputs outs)]
    ;;(println "add node " id)
    (swap! nodes assoc kw node)
    (doseq [b (concat ins outs)]
      (let [wpoint {:point b :node node}
            wid (config b :id)]
        (swap! points assoc wid wpoint)
        (listen b :action
                (fn [e]
                  (if-let [lid (:last-point @s-panel)]
                    (let [lpoint (get @points lid)]
                      (make-connection lid lpoint wid wpoint)
                      (let [lp (:point lpoint)
                            bc (default-color "Button.background")]
                        ;;(println "lp = " lp " bc = " bc)
                        (config! lp :background bc)))
                    (do
                      (swap! s-panel assoc :last-point wid)
                      (config! b :background :blue)))))))
    (config! (:panel @s-panel)
             :items (conj (config (:panel @s-panel) :items)
                          (:widget node)))
    node))

(defn get-id [t e]
  (let [id (if (number? e) e (swap! next-id inc))]
    (str t ":" id)))

(defn kill-synth [n]
  (if-let [synth (:synth n)]
    (s/skill synth)))

(defn- osc [name synth-type]
  (let [f (fn [] (make-synth synth-type))]
    (add-node :name name :play-fn f :input "freq" :output "sig" :out-type :audio :synth-type synth-type )))

(defn saw-osc [e]
  (let [id (get-id "saw-osc" e)]
    (osc id s/saw-osc)))

(defn square-osc [e]
  (let [id (get-id "square-osc" e)
        f (fn [] (make-synth s/square-osc))]
    (add-node :name id :play-fn f :input "freq" :output "sig" :out-type :audio :synth-type s/square-osc)))

(defn sin-osc [e]
  (let [id (get-id "sin-osc" e)]
    (osc id s/s_sin-osc)))

(defn lp-filt [e]
  (let [id (get-id "lp-filt" e)
        f (fn [] (make-synth s/lp-filt))]
    (add-node :name id :play-fn f :input "in" :output "out" :out-type :audio :synth-type s/lp-filt )))

(defn hp-filt [e]
  (let [id (get-id "hp-filt" e)
        f (fn [] (make-synth s/hp-filt))]
    (add-node :name id :play-fn f :input "in" :output "out" :out-type :audio :synth-type s/hp-filt) ))

(defn bp-filt [e]
  (let [id (get-id "bp-filt" e)
        f (fn [] (make-synth s/bp-filt))]
    (add-node :name id :play-fn f :input "in" :output "out" :out-type :audio :synth-type s/bp-filt) ))

(defn moog-filt [e]
  (let [id (get-id "moog-filt" e)
        f (fn [] (make-synth s/moog-filt))]
    (add-node :name id :play-fn f :input "in" :output "out" :out-type :audio :synth-type s/moog-filt) ))

(defn freeverb [e]
  (let [id (get-id "freeverb" e)
        f (fn [] (make-synth s/freeverb))]
    (add-node :name id :play-fn f :input "in" :output "out" :out-type :audio :synth-type s/freeverb )))

(defn echo [e]
  (let [id (get-id "echo" e)
        f (fn [] (make-synth s/echo))]
    (add-node :name id :play-fn f :input "in" :output "out" :out-type :audio :synth-type s/echo) ))

(defn amp [e]
  (let [id (get-id "amp" e)
        f (fn [] (make-synth s/amp))]
    (add-node :name id :play-fn f :input "in" :output "out" :out-type :audio :synth-type s/amp) ))

(defn pct-add [e]
  (let [id (get-id "pct-add" e)
        f (fn [] (make-synth s/pct-add))]
    (add-node :name id :play-fn f :input "in" :output "out" :out-type :control :synth-type s/pct-add) ))

(defn sin-vco [e]
  (let [id (get-id "sin-vco" e)
        f (fn []  (make-synth s/sin-vco))]
    (add-node :name id :play-fn f :input "freq" :output "out" :out-type :control :synth-type s/sin-vco)))

(defn note-in [e]
  (let [id (get-id "note-in" e)
        f (fn []  (make-synth s/midi-in))]
    (add-node :name id :play-fn f :input "note" :output "freq" :out-type :control :synth-type s/midi-in)))

(defn audio-out [e]
  (let [id (get-id "audio-out" e)
        f (fn []  (make-synth s/audio-out))]
    (add-node :name id :play-fn f :output "out" :out-type :audio :synth-type s/audio-out)))

(defn c-splitter [e]
  (let [id (get-id "c-splitter" e)
        f (fn []  (make-synth s/c-splitter))]
    (add-node :name id :play-fn f :output :split :out-type :control :synth-type s/c-splitter)))

(defn a-splitter [e]
  (let [id (get-id "a-splitter" e)
        f (fn [] (make-synth s/a-splitter))]
    (add-node :name id :play-fn f :output :split :out-type :audio :synth-type s/a-splitter)))

(defn a-mixer-2 [e]
  (let [id (get-id "a-mixer-2" e)
        f (fn [] (make-synth s/a-mixer-2))]
    (add-node :name id :play-fn f :output "out" :out-type :audio :synth-type s/a-mixer-2)))

(defn a-mixer-4 [e]
  (let [id (get-id "a-mixer-4" e)
        f (fn [] (make-synth s/a-mixer-4))]
    (add-node :name id :play-fn f :output "out" :out-type :audio :synth-type s/a-mixer-4)))

(defn const [e]
  (let [id (get-id "const" e)
        t (text :text "     " :columns 5 :id (str id "-text"))
        f (fn [] (let [synth (s/const)
                      v (text t)
                      unregister (listen t :action (fn [e] (s/sctl synth :ibus (read-string (text t)))))]
                  (if (not (empty? (str/trim v)))
                    (s/sctl synth :ibus (read-string v)))
                  {:synth synth :stop-fn unregister}))]
    (add-node :name id :play-fn f :output "val" :out-type :control :synth-type s/const
              :cent t)))

(defn midi-in [e]
  (let [id (get-id "midi-in" e)
        t (text :text "" :columns 1)
        f (fn [] (let [synth (s/midi-in)
                      unregister (listen t :key-pressed (fn [e]
                                           (let [n (min 127 (.getKeyCode e))]
                                             (println n)
                                             (s/sctl synth :note n))))]
                  {:synth synth :stop-fn unregister}))]
    (add-node :name id :output "freq" :out-type :control :cent t :play-fn f)))

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

(defn get-value-field
  [w kw]
  (select w [(keyword (str "#" (name kw) "-text"))]))

(defn get-if-value
  [w kw]
  (if-let [vw (get-value-field w kw)]
    (read-string (config vw :text))
    nil))

(defn dump-nodes []
  (map (fn [e]
         (let [w (:widget e)
               l (config w :location)
               kw (config w :id)
               rv {:x (.getX l) :y (.getY l) :w kw}
               v (if-let [vw (get-if-value w kw)]
                   (assoc rv :v vw)
                   rv)]
           v))
       (vals @nodes)))

(defn dump-synth [s]
  (symbol (second (first s))))

(defn dump-connections []
  @connections)

(defn dump-all []
  {:nodes (symbol (str "'" (to-string (dump-nodes))))
   :connections (symbol (str "'" (to-string (dump-connections))))
   :master-vol (:master-vol @s-panel)})

(defn make-node [ntype id x y v]
  (let [s (str "(" ntype " " id ")")
        m (load-string s)
        w (:widget m)
        kw (config w :id)]
    (if-let [vw (get-value-field w kw)]
      (config! vw :text v))
    (move! (:widget m) :by [x y])
    m))



(defn restore-node
  [n node-map]
  (let [name (name n)
        k (keyword (get-node-name name))
        m (get node-map k)
        b (select (:widget m) [(keyword (str "#" name))])]
    {:point b :node m}))

(defn partition-with
  "partitions all elements to two buckets where first is pred is true, second is pred is false

   this is how I always think of partition-by working, but it has a different behavior."
  [pred coll]
  [(filter pred coll)
   (filter (complement pred) coll)])

(defn get-starts
  "n is (:connections (load-file xxx.clj))"
  [n]
  (let [cn (for [[f t] n]
             [(get-node-name f) (get-node-name t)])
        tos (into #{} (for [[f t] cn] t))]
    (filter #(not (contains? tos %)) (for [[f t] cn] f))))

(defn get-chain [s1 n]
  (let [[neighbors others] (partition-with #(= s1 (get-node-name (first %))) n)]
    (if (empty? neighbors)
      s1
      [s1 (for [[f t] neighbors] (get-chain (get-node-name t) others))])))

(defn get-order [n]
  (let [s (get-starts n)
        l (flatten (for [s1 s]
                     (get-chain s1 n)))]
    (map #(keyword %) (distinct l))))


(defn build-synths-and-connections
  [ordered-nodes connections make-new-connections]
  ;;(swap! s-panel assoc :run-mode true)
  (when (:run-mode @s-panel)
    (doseq [n1 ordered-nodes]
      (let [node (get @nodes n1)
            synth ((:play-fn node))
            node1 (conj node synth)]
        (swap! nodes assoc n1 node1))))
  (doseq [[n1 n2] connections]
    (let [node1 (restore-node n1 @nodes)
          node2 (restore-node n2 @nodes)]
      (if make-new-connections
          (do
            (make-connection n1 node1 n2 node2)
            (swap! s-panel assoc :last-point n2))
          (let [c (connect-points node1 node2)]
            (swap! busses assoc (:name c) c))))))

(defn kill-running-synths [ordered-nodes]
  (doseq [n1 ordered-nodes]
    (let [node (get @nodes n1)]
      (kill-synth node)
      (if-let [stop-fn (:stop-fn node)]
        (stop-fn)))))

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

(defn sound-on [e]
  (s/svolume (:master-vol @s-panel))
  (swap! s-panel assoc :run-mode true)
  (build-synths-and-connections (get-order @connections) @connections false))

(defn sound-off [e]
  (kill-running-synths (reverse (get-order @connections)))
  (swap! s-panel assoc :run-mode false)
  (s/svolume 0.0))

(defn ms-reset! []
  (reset! connections [])
  (reset! points {})
  (reset! nodes {})
  (reset! busses {})
  (reset! next-id 0)
  (sound-off 0))

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
                                             (action :handler pct-add :name "Pct Add")
                                             (action :handler slider-ctl :name "Slider")
                                             (action :handler c-splitter :name "Control Splitter")
                                             (action :handler a-splitter :name "Audio Splitter")
                                             (action :handler a-mixer-2 :name "2 Ch mixer")
                                             (action :handler a-mixer-4 :name "4 Ch mixer")
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
    (swap! s-panel assoc :run-mode false)
    (config! f :on-close :dispose)
    (-> f bugger-what! show!)))

(defn restore
  "usage: (restore (load-file fff.clj))

   strangely, it matters what order the nodes are created in. My best
   guess, which seems to work is that they need to more or less be added
   from source to sink. So starting with the freq gens, to the oscillators,
   filters to the audio out. I don't know why this is true

   TODO: What I really need to do is have an edit mode so the user can put the
   nodes on the canvas in any order, then when the sound-on is hit, create
   the synths in the right order irrespective of how the user put them in.
"
  [r]
  (ms-reset!)
  (swap! s-panel assoc :master-vol (:master-vol r))
  (let [f (-main)
        o (get-order (:connections r))
        node-map (into {}  (for [n1 (:nodes r)] [(:w n1) n1]))
        m (into {}
           (for [n1 o]
             (let [n (get node-map n1)
                   s (str/split (name (:w n)) #":")
                   wname (first s)
                   wnum  (second s)
                   x (:x n)
                   y (:y n)
                   v (:v n)]
               ;;(println n s wname wnum x y)
               [(:w n) (make-node wname wnum x y v)])))]
    (build-synths-and-connections o (:connections r) true))
  (swap! s-panel assoc :last-point  nil))
