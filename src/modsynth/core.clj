;  Copyright (c) Bill Allen, 2013. All rights reserved.

;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this
;   distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns modsynth.core
  (:use [seesaw core border behave graphics color chooser table]
        [modsynth piano join]
        [clojure.pprint :only [write]])
  (:require [modsynth.synths :as s]
            [modsynth.midi :as m]
            [clojure.string :as str]
            [overtone.core :refer [ctl volume kill at now]]
            [overtone.studio.scope :refer [pscope]])
  (:import [javax.swing SwingUtilities]
           [java.awt Color]))

(native!)
(def s-panel (atom {}))
(def next-id (atom 0))
(def nodes (atom {}))
(def points (atom {}))
(def connections (atom []))
(def gated-synths (atom #{}))
(def busses (atom {}))

(def color-control "#AAFFFF")
(def color-audio "#FFBBFF")

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
     (doseq [[id1 id2] @connections]
       (let [w1 (get @points id1)
             w2 (get @points id2)
             color (if (= (get-in @points [id1 :node :out-type]) :control) color-control color-audio)
             [x1 y1 x2 y2] (concat (getXY w1) (getXY w2))]
         ;(println x1 y1 x2 y2)
         (.setColor g (Color/decode color))
         (.drawLine g x1 y1 x2 y2)))
     (.setColor g cur-color)
    ))

(defn get-synth-controls [s]
  (filter #(not (contains? #{"obus" "ibus" "gate"} %)) (map :name (:params s))))

(defn has-gate [s]
  (let [f (filter #(contains? #{"gate"} %) (map :name (:params s)))]
    (not (empty? f))))

(defn get-split-controls
  [s t]
  (if (= t :input)
    (filter #(not (contains? #{"ob1" "ob2" "ibus"} %)) (map :name (:params s)))
    (filter #(contains? #{"ob1" "ob2"} %) (map :name (:params s)))))

(defn make-io
  [io t node-id otype]
  ;;(println t otype)
  (if (= otype :split) ; special case code for splitters
    (if (= t :input)
      (cons (button :text "in" :class t :id (str node-id "-in"))
            (for [c (get-split-controls (:synth-type io) :input)]
              (button :text c :class t :id (str node-id "-" c))))
      (for [c (get-split-controls (:synth-type io) :output)]
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

(defn make-synth [s]
  (println "make synth " (:name s))
  (let [synth (s)]
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
  (if (:run-mode @s-panel)
    (alert "no editing while sound on")
    (do
      (cond ;; if the two points are connected, disconnect them.
       (connected? lid wid) (reset! connections (filter #(not= % [lid wid]) @connections))
       (connected? wid lid) (reset! connections (filter #(not= % [wid lid]) @connections))
       :else (let [lpn (get-node-name lid)
                   wpn (get-node-name wid)]
               (do
                 (when (not= lpn wpn) ; don't connect points of same node
                   (swap! connections conj [lid wid])))))
      (swap! s-panel assoc :last-point  nil :changes-pending true))))

"
Each element on the screen is a node. Each node represents a visual widget and a synth or a manual controller like a slider. Every node
is stored in a map nodes keyed by its id.

Each connection point is a button of a node that represents some parameter of the node's synth.
Connections are references to two connection points
"

(defn add-node [& ps]
  (if (:run-mode @s-panel)
    (alert "no editing while sound on")
    (let [io (apply hash-map ps)
          id (:name io)
          kw (keyword id)
          otype (:output io)
          ins  (make-io io :input id otype)
          outs  (make-io io :output id otype)
          gated (has-gate (:synth-type io))
          color (if (= (:out-type io) :control) color-control color-audio)
          node (assoc io :widget (doto (border-panel :id id
                                                     :border (line-border :top 1 :color color)
                                                     :north (label :text id :background color :h-text-position :center)
                                                     :center (if (= nil (:cent io)) (label "") (:cent io))
                                                     :east (grid-panel :rows (count outs) :columns 1 :items outs)
                                                     :west (grid-panel :rows (count ins) :columns 1 :items ins)
                                                     )
                                   (config! :bounds :preferred)
                                   movable)
                      :id kw :inputs ins :outputs outs :gated gated)]
      ;;(println "add node " id)
      (swap! nodes assoc kw node)
      (swap! s-panel assoc :changes-pending true)
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
      node)))

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

(defn adsr-env [e]
  (let [id (get-id "adsr-env" e)
        f (fn [] (make-synth s/adsr-env))]
    (add-node :name id :play-fn f :input "in" :output "out" :out-type :audio :synth-type s/adsr-env) ))

(defn perc-env [e]
  (let [id (get-id "perc-env" e)
        f (fn [] (make-synth s/perc-env))]
    (add-node :name id :play-fn f :input "in" :output "out" :out-type :audio :synth-type s/perc-env) ))

(defn pct-add [e]
  (let [id (get-id "pct-add" e)
        f (fn [] (make-synth s/pct-add))]
    (add-node :name id :play-fn f :input "in" :output "out" :out-type :control :synth-type s/pct-add) ))

(defn val-add [e]
  (let [id (get-id "val-add" e)
        f (fn [] (make-synth s/val-add))]
    (add-node :name id :play-fn f :input "in" :output "out" :out-type :control :synth-type s/val-add) ))

(defn sin-vco [e]
  (let [id (get-id "sin-vco" e)
        f (fn []  (make-synth s/sin-vco))]
    (add-node :name id :play-fn f :input "freq" :output "out" :out-type :control :synth-type s/sin-vco)))

(defn rand-in [e]
  (let [id (get-id "rand-in" e)
        f (fn []  (make-synth s/rand-in))]
    (add-node :name id :play-fn f :output "out" :out-type :control :synth-type s/rand-in)))

(defn rand-pent [e]
  (let [id (get-id "rand-pent" e)
        f (fn []  (make-synth s/rand-pent))]
    (add-node :name id :play-fn f :input "val" :output "out" :out-type :control :synth-type s/rand-pent)))

(defn note-in [e]
  (let [id (get-id "note-in" e)
        f (fn []  (make-synth s/note-in))]
    (add-node :name id :play-fn f :output "freq" :out-type :control :synth-type s/note-in)))

(defn audio-out [e]
  (let [id (get-id "audio-out" e)
        f (fn []  (make-synth s/audio-out))]
    (add-node :name id :play-fn f :output "out" :out-type :audio :synth-type s/audio-out)))

(defn audio-in [e]
  (let [id (get-id "audio-in" e)
        f (fn []  (make-synth s/audio-in))]
    (add-node :name id :play-fn f :output "out" :out-type :audio :synth-type s/audio-in)))

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

(defn slider-ctl [e]
  (let [id (get-id "slider-ctl" e)
        s (slider :value 0 :min 0 :max 100 :orientation :vertical)
        f (fn [] (let [synth (s/const)
                      unregister (listen s :change (fn [e] (s/sctl synth :ibus (int (value s)))))]
                  (s/sctl synth :ibus (int (value s)))
                  {:synth synth :stop-fn unregister}))]
    (add-node :name id :play-fn f :output "val" :out-type :control :synth-type s/const :cent s)))

(defn doc-node
  ([e] (doc-node e 8))
  ([e rows]
     (let [id (get-id "doc-node" e)
           t (scrollable (text :multi-line? true :editable? true :wrap-lines? true :columns 20 :rows (+ 2 rows) :id (str id "-text")))]
       (add-node :name id :cent t))))

(defn play-note [synth n]
  (s/sctl synth :note n)
  (doseq [gated-synth @gated-synths]
    (s/sctl gated-synth :gate 0)
    (at (+ (now) 50) (s/sctl gated-synth :gate 1))))

(defn shut-gates []
  (doseq [gated-synth @gated-synths]
    (s/sctl gated-synth :gate 0)))

(defn midi-in2 [e]
  (let [id (get-id "midi-in2" e)
        t (text :text "" :columns 1)
        f (fn [] (let [synth (s/midi-in)
                      unregister (listen t :key-pressed (fn [e]
                                           (let [n (min 127 (.getKeyCode e))]
                                             (println n)
                                             (play-note synth n))))]
                  {:synth synth :stop-fn unregister}))]
    (add-node :name id :output "freq" :out-type :control :cent t :play-fn f)))

(defn midi-in [e]
  (let [id (get-id "midi-in" e)
        f (fn [] (let [synth (s/midi-in)]
                  (do
                    (m/register-note-events synth)
                    {:synth synth})))]
    (add-node :name id :output "freq" :out-type :control :play-fn f)))

(defn cc-cont-in [e]
  (let [id (get-id "cc-cont-in" e)
        t (text :text "  " :columns 2 :id (str id "-text"))
        f (fn [] (let [synth (s/cc-in)
                      v (text t)
                      unregister (listen t :action (fn [e] (s/sctl synth :ibus (read-string (text t)))))]
                  (do
                    (if (not (empty? (str/trim v)))
                      (m/register-continuous-cc-events synth (read-string v)))
                    {:synth synth :stop-fn unregister})))]
    (add-node :name id :output "val" :out-type :control :play-fn f :cent t)))

(defn disc-panel [id]
  (let  [t (text :text "  " :columns 4 :id (str id "-text"))
         l (label :text "0")
         p (border-panel :center t :south l)]
    [t l p]))

(defn cc-disc-in [e]
  (let [id (get-id "cc-disc-in" e)
        [t l p] (disc-panel id)
        wf (fn [v] (config! l :text (str v)))
        rf (fn [] (int (read-string (config l :text))))
        f (fn [] (let [synth (s/cc-in)
                      v (text t)
                      unregister (listen t :action (fn [e] (s/sctl synth :ibus (read-string (text t)))))]
                  (do
                    (if (not (empty? (str/trim v)))
                      (m/register-discreet-cc-events synth (read-string v) wf rf))
                    {:synth synth :stop-fn unregister})))]
    (add-node :name id :output "val" :out-type :control :play-fn f :cent p)))


(defn piano-in [e]
  (let [id (get-id "piano-in" e)
        f (fn []  (let [synth (s/midi-in)
                       p (piano (fn [k] (play-note synth k))
                                (fn [k] (shut-gates)))]
                   (show! p)
                   {:synth synth}))]
    (add-node :name id :play-fn f :output "freq" :out-type :control)))



(defn to-string [e]
  (write e :stream nil :pretty false))

(defn get-value-field
  [w kw]
  (if-let [w (select w [(keyword (str "#" (name kw) "-text"))])]
    (if (or (nil? w) (empty? (str/trim w))) nil w)))

(defn get-if-value
  [w kw]
  (if-let [vw (get-value-field w kw)]
    (let [t (config vw :text)]
      (if (.startsWith (get-node-name kw) "doc")
        t
        (read-string t)))
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

(defn get-frame-dimension [f]
  (let [d (.getSize f)
        h (.getHeight d)
        w (.getWidth d)]
    {:height h :width w}))

(defn dump-all []
  {:nodes (symbol (str "'" (to-string (dump-nodes))))
   :connections (symbol (str "'" (to-string (dump-connections))))
   :master-vol (:master-vol @s-panel)
   :frame (get-frame-dimension (:frame @s-panel))})

(defn make-node [ntype id x y v]
  (let [rc (if (= ntype "doc-node") (count (filter #(= \newline %) v)))
        s (str "(modsynth.core/" ntype " " id " " rc ")")
        m (load-string s)
        w (:widget m)
        kw (config w :id)]
    (if-let [vw (get-value-field w kw)]
      (config! vw :text v))
    (move! (:widget m) :by [x y])
    (reset! next-id (max @next-id (load-string id)))
    m))



(defn restore-node
  [n node-map]
  (let [name (name n)
        k (keyword (get-node-name name))
        m (get node-map k)
        b (select (:widget m) [(keyword (str "#" name))])]
    {:point b :node m}))


(defn get-ends
  "n is (:connections (load-file xxx.clj))"
  [n]
  (let [cn (for [[f t] n]
             [(get-node-name f) (get-node-name t)])
        tos (into #{} (for [[f t] cn] f))
        m (filter #(not (contains? tos %)) (for [[f t] cn] t))]
    (into #{} (flatten
               (map (fn [e] (filter #(.startsWith (name %) e) (map second n))) m)))))


(defn instantiate-synths
  [ordered-nodes]
  (doseq [n1 ordered-nodes]
      (let [node (get @nodes n1)
            synth ((:play-fn node))
            node1 (conj node synth)]
        (if (:gated node)
          (swap! gated-synths conj (:synth synth)))
        (swap! nodes assoc n1 node1))))

(defn find-implied-connections [c]
  (let [candidates
        (for [[_ f] c]
          (for [[t _] c
                :let [x [f t]]
                :when (= (get-node-name t) (get-node-name f))]
            x))]
    (map vec (partition 2 (flatten candidates)))))

(defn add-implied-connections [c]
  (concat c (find-implied-connections c)))

(defn remove-implied-connections [c]
  (filter (fn [[t f]] (not= (get-node-name t) (get-node-name f))) c))

(defn get-connection-map
  [connections]
  (loop [c connections r {}]
    (if (empty? c) r
        (let [e (first c)
              k (first e)
              b (k r)]
          (recur (rest c) (assoc r k (if b (vec (concat b [e])) [e])))))))

(defn build-tree [cc e]
  (if (nil? e) nil
      (let [r (map first (filter #(= (second %) e) cc))]
        (cons e (for [ee r] (build-tree cc ee))))))

(defn bf [& roots]
   (if (seq roots)
       (concat (map first roots) ;; values in roots
               (apply bf (mapcat rest roots)))))

(defn get-order [n]
  "adds implied connections to existing connections n then gets
   all the points (i.e. every point on each node) in rough order
   on the chain"
  (let [c1 (add-implied-connections n)
        c2 (for [n (get-ends c1)]
             [n (keyword (str (get-node-name n) "-out"))]) ; adds implied end points
        c (concat c1 c2)
        t (build-tree c (first (into #{} (map second c2))))]
    (reverse (bf t))))


(defn build-synths-and-connections
  [ordered-nodes connections make-new-connections]
  (let [connection-map (get-connection-map connections)]
   (doseq [node ordered-nodes]
     (if-let [[[n1 n2]] (get connection-map node) ]
       (do
         (println n1 n2)
         (let [node1 (restore-node n1 @nodes)
               node2 (restore-node n2 @nodes)]
          (if make-new-connections
            (do
              (make-connection n1 node1 n2 node2)
              (swap! s-panel assoc :last-point n2))
            (let [c (connect-points node1 node2)]
              (swap! busses assoc (:name c) c)))))))))

(defn kill-running-synths [ordered-nodes]
  (doseq [n1 ordered-nodes]
    (let [node (get @nodes n1)]
      (kill-synth node)
      (if-let [stop-fn (:stop-fn node)]
        (stop-fn)))))


(defn get-node-order [o]
  (distinct (map (comp keyword get-node-name) o)))

(defn sound-on [e]
  (s/svolume (:master-vol @s-panel))
  (swap! s-panel assoc :run-mode true)
  (let [ordered-nodes (get-order @connections)]
    (m/init)
    (instantiate-synths (get-node-order ordered-nodes))
    (build-synths-and-connections ordered-nodes @connections false)))

(defn sound-off [e]
  (when (:run-mode @s-panel)
    (kill-running-synths (reverse (get-node-order (get-order @connections))))
    (swap! s-panel assoc :run-mode false)
    (s/svolume 0.0)))

(defn ms-reset! []
  (sound-off 0)
  (reset! connections [])
  (reset! gated-synths #{})
  (reset! points {})
  (reset! nodes {})
  (reset! busses {})
  (reset! next-id 0)
  (reset! s-panel {}))

(defn make-panel []
  (xyz-panel
    :paint draw-grid
    :id :xyz
    :background "#222222"
    :items []
    ))

(declare ms-new
         ms-load-file
         ms-save-file
         ms-save-file-as
         ms-load-example
         monitor-all-busses)

(defn fr []
  (let [p (make-panel)]
    (swap! s-panel assoc :panel p :last-point nil :master-vol 0.3)
    (frame
     :menubar (menubar :items [(menu :text "File"
                                     :items [(action :handler ms-new :name "New")
                                             (action :handler ms-load-file :name "Load File")
                                             (action :handler ms-load-example :name "Load Example")
                                             (action :handler ms-save-file :name "Save File")
                                             (action :handler ms-save-file-as :name "Save File As")
                                             (action :handler dispose! :name "Exit")])
                               (menu :text "Control"
                                     :items [(action :handler monitor-all-busses :name "Buss Monitor")
                                             (action :handler sound-on :name "Sound On")
                                             (action :handler sound-off :name "Sound Off")])
                               (menu :text "Modules"
                                     :items [(menu :text "Input/Output"
                                                   :items [(action :handler midi-in :name "Midi In")
                                                           (action :handler midi-in2 :name "Midi In 2")
                                                           (action :handler cc-cont-in :name "CC Continuous")
                                                           (action :handler cc-disc-in :name "CC Discreet")
                                                           (action :handler note-in :name "Note In")
                                                           (action :handler piano-in :name "Piano In")
                                                           (action :handler audio-out :name "Audio Out")
                                                           ])
                                             (menu :text "Oscillators"
                                                   :items [(action :handler saw-osc :name "Saw Osc")
                                                           (action :handler square-osc :name "Square Osc")
                                                           (action :handler sin-osc :name "Sin Osc")
                                                           (action :handler sin-vco :name "Sin VCO")
                                                           ])
                                             (menu :text "Filters"
                                                   :items [(action :handler lp-filt :name "LP Filt")
                                                           (action :handler hp-filt :name "HP Filt")
                                                           (action :handler bp-filt :name "BP Filt")
                                                           (action :handler moog-filt :name "Moog Filt")
                                                           ])
                                             (menu :text "Effects"
                                                   :items [(action :handler freeverb :name "Freeverb")
                                                           (action :handler echo :name "Echo")
                                                           (action :handler amp :name "Amp")
                                                           (action :handler adsr-env :name "ADSR-Env")
                                                           (action :handler perc-env :name "Perc-Env")
                                                           ])
                                             (menu :text "Mixing"
                                                   :items [(action :handler a-splitter :name "Audio Splitter")
                                                           (action :handler a-mixer-2 :name "2 Ch mixer")
                                                           (action :handler a-mixer-4 :name "4 Ch mixer")
                                                           ])
                                             (menu :text "Control"
                                                   :items [(action :handler pct-add :name "Pct Add")
                                                           (action :handler val-add :name "Val Add")
                                                           (action :handler slider-ctl :name "Slider")
                                                           (action :handler c-splitter :name "Control Splitter")                                                           ])
                                             (menu :text "Miscellaneous"
                                                   :items [(action :handler rand-in :name "Random Val")
                                                           (action :handler rand-pent :name "Random Pentatonic")
                                                           (action :handler const :name "Const")
                                                           (action :handler doc-node :name "Doc Node")])])])
     :title   "Overtone Modular Synth"
     :content (border-panel
               :vgap 5
               :center p)
     :size    [600 :by 600])))

(defn bugger-what!
  "for some reason, makes the frame show full size, sometimes
   swing has a high astonishment factor
  "
  [f]
  (if (= (java.awt.Dimension.) (.getSize f))
    (pack! f)
    f))

(defn get-buss-order [c]
  (let [o (get-order c)
        cm (get-connection-map c)]
    (for [oo o
          :let [v (oo cm)]
          :when v]
      (let [[[f t]] v] (str (name f) " -> " (name t))))))

(defn monitor-all-busses [e]
  (future
    (let [bm (all-bus-monitors)
          v (fn [bm]
              (let [bo (map (fn [e] [e (get bm e)]) (get-buss-order @connections))]
                (for [[k b] bo] {:name k :val (deref b)})))
          c [:name :val]
          t (table :model [:columns c :rows (v bm)])
          f (frame :title "Buss Monitor"
                   :content (border-panel :center t))]
      (-> f bugger-what! show!)
      (loop []
        (Thread/sleep 200)
        (let [bm1 (all-bus-monitors)
              model (table-model :columns c :rows (v bm1))]
          (config! t :model model))
        (recur)))))


(defn -main [& args]
  (ms-reset!)
  (let [f (invoke-now (fr))]
    (swap! s-panel assoc :frame f :run-mode false :changes-pending false)
    (config! f :on-close :dispose)
    (-> f bugger-what! show!)))

(defn set-frame-size [f d]
  (when d
    (.setSize f (java.awt.Dimension. (:width d) (:height d)))))

(defn restore
  "usage: (restore (load-file fff.clj))

   strangely, it matters what order the nodes are created in. My best
   guess, which seems to work is that they need to more or less be added
   from source to sink. So starting with the freq gens, to the oscillators,
   filters to the audio out. I don't know why this is true, but to make
   it work, we attempt to work out the right order of node creation using
   get-order on the connections.
"
  [r]
  (ms-reset!)
  (println "s-panel type is " (type @s-panel))
  (swap! s-panel assoc :master-vol (:master-vol r))
  (let [f (-main)
        c (:connections r)
        oc (get-order c)
        o (distinct (map (comp keyword get-node-name) oc))
        node-map (into {}  (for [n1 (:nodes r)] [(:w n1) n1]))
        r-make-node (fn [n] (let [s (str/split (name (:w n)) #":")
                                 wname (first s)
                                 wnum  (second s)
                                 x (:x n)
                                 y (:y n)
                                 v (:v n)]
                             (println n s wname wnum x y)
                             (make-node wname wnum x y v)))]
    (doseq [n1 o]
      (let [n (get node-map n1)]
        [(:w n) (r-make-node n)]))
    (doseq [n (filter #(.startsWith (get-node-name (:w %)) "doc") (:nodes r))]
      (r-make-node n))
    (build-synths-and-connections (distinct oc) c true)
    (set-frame-size f (:frame r)))
  (swap! s-panel assoc :last-point  nil :changes-pending false))

(defn ms-new
  [e]
  (hide! (:frame @s-panel))
  (-main))

(defn ms-load-example [e]
  (ms-load-file e "./examples"))

(defn ms-load-file
  ([e] (ms-load-file e (java.lang.System/getProperty "user.dir")))
  ([e dir] (let [cont? (if (:changes-pending @s-panel)
                         (not (confirm (:panel @s-panel) "edits pending - save?" :option-type :yes-no))
                         true)]
             (when cont?
               (let [file (choose-file (:panel @s-panel)
                                       :dir dir
                                       :filters [["Mod Synths" ["oms"]]]
                                       :success-fn (fn [fc file] (.getAbsolutePath file)))]
                 (hide! (:frame @s-panel))
                 (restore (load-file file)))))))

(defn ms-save-file
  ([e] (ms-save-file e false))
  ([e is-as]
     (if (or is-as (:changes-pending @s-panel))
       (let [dir (java.lang.System/getProperty "user.dir")
             file (choose-file (:panel @s-panel)
                               :type :save
                               :dir dir
                               :filters [["Mod Synths" ["oms"]]]
                               :success-fn (fn [fc file] (.getAbsolutePath file)))]
         (when file
           (spit file (dump-all))
           (swap! s-panel :changes-pending false)))
       (alert "no changes to be saved"))))

(defn ms-save-file-as [e]
  (ms-save-file e true))
