(ns modsynth.synths
  (:use overtone.live)  ; to work on gui without overtone comment out this line
  )

;;(defmacro mod-defsynth [name p body] (let [sym-name (symbol (eval name))] `(o/defsynth ~sym-name ~p ~body)))
;;(defmacro mod-ctl [name t val] (let [sym-name (symbol (eval name))] `(o/ctl ~sym-name ~t ~val)))

;; to work on gui without overtone uncomment these lines
;; ******
;; (defmacro defsynth [name p body] `(defn ~name [] (comment ~p) ~name))
;; (defn control-bus [x])
;; (defn audio-bus [x])
;; (defn ctl [x y z])
;; (defn volume [x])
;; *******

(def OB 2)
(def OB1 OB)
(def OB2 (inc OB))
(def IB (+ OB 1))
(def B1 (+ IB 1))
(def B2 (inc B1))
(def B3 (inc B2))
(def B4 (inc B3))

(defsynth audio-out
  [obus 0
   b1 OB1
   b2 OB2]
  (out obus [(in:ar b1 1) (in:ar b2 1) ]))

(defsynth audio-in
  [obus OB
   ibus 0]
  (out obus [(sound-in:ar ibus 1)]))

(defsynth c-splitter
  [ob1 OB1
   ob2 OB2
   ibus IB]
  (let [input (in:kr ibus 1)]
    (out ob1 input)
    (out ob2 input)))

(defsynth a-splitter
  "pos is 0 full left 50 middle 100 full right
   lev is 0 to 100"
  [ob1 OB1
   ob2 OB2
   ibus IB
   pos {:default B1 :def 50 :max 100 :min 0 :step 1}
   lev {:default B1 :def 50 :max 100 :min 0 :step 1}]
  (let [input (in:ar ibus 1)
        position (- (* (in:kr pos 1) 0.02) 1.0)
        level (* (in:kr lev 1) 0.01)
        ;;p (pan2 input position level)
        ;; do it by hand instead of with pan2
        tpos (+ 0.5 (* position 0.5))
        ]
    (out ob1 (* input level tpos))
    (out ob2 (* input level (- 1 tpos)))))

(defsynth a-mixer-2
  [obus OB
   in1 B1
   in2 B2]
  (let [input1 (in:ar in1 1)
        input2 (in:ar in2 1)]
    (out obus (+ input1 input2))))

(defsynth a-mixer-4
  [obus OB
   in1 B1
   in2 B2
   in3 B3
   in4 B4]
  (let [input1 (in:ar in1 1)
        input2 (in:ar in2 1)
        input3 (in:ar in3 1)
        input4 (in:ar in4 1)]
    (out obus (+ input1 input2 input3 input4))))

(defsynth const
  [obus OB
   ibus 0]
  (out obus (* ibus 1.0)))

(defsynth cc-in
  [obus OB
   ibus IB]
  (out:kr obus ibus))

(defsynth midi-in
  [obus OB
   note 60]
  (out:kr obus note))

(defsynth note-freq
  [obus OB
   note IB]
  (let [freq (midicps (in:kr note 1))]
    (out:kr obus freq)))

;; (defonce index-buffer
;;   (let [buf (buffer 128)
;;         data (take-while #(< % 100) (drop-while #(> % 30) (scale-field :a :pentatonic)))]
;;     (doseq [[idx val] (map-indexed (fn [a b] [a b]) data)]
;;       (buffer-set! buf idx val))
;;     buf))

;; (defsynth rand-pent
;;   [obus OB
;;    ibus IB]
;;   (out:kr obus (index:kr index-buffer (in:kr ibus 1))))

(defsynth saw-osc
  [obus OB
   ibus IB]
  (let [freq (in:kr ibus 1)]
    (out obus (saw freq))))

(defsynth s_sin-osc
  [obus OB
   ibus IB]
  (let [freq (in:kr ibus 1)]
    (out obus (sin-osc freq))))

(defsynth sin-vco
  "normalizes to output a sine wave that goes from 0 to 100"
  [obus OB
   ibus IB]
  (out obus (* 50 (+ 1.0 (sin-osc:kr (in:kr ibus 1))))))

(defsynth rand-in
  [obus OB
   lo {:default B1 :def 0 :min 0 :max 10000 :step 1}
   hi {:default B2 :def 0 :min 0 :max 10000 :step 1}
   trig {:default B3 :def 0 :min 0 :max 1 :step 1}]
  (let [low (in:kr lo 1)
        high (in:kr hi 1)
        trigger (in:kr trig 1)]
   (out obus (t-rand:kr low high trigger))))

;; this is an abuse-of-sorts of the parameter system for defsynths. For
;; modsynth synths, the default has to be a unique bus number. The other
;; parameters give the range of the values that this bus should receive.
(defsynth square-osc
  [obus OB
   ibus IB
   width {:default B1 :def 50 :max 50 :min 10 :step 1}]
  (let [freq (in:kr ibus 1)
        w (+ 0.1 (* (in:kr width 1) 0.004))]
    (out obus (pulse freq w))))

;; (defsynth square-osc
;;   [obus OB
;;    ibus IB
;;    width {:default 50 :def :min 5 :max 50 :step 1}]
;;   (let [freq (in:kr ibus 2)]
;;     (out obus (pulse freq (/ width 100.0)))))

(defsynth lp-filt
  [cutoff {:default B1 :def 300 :min 30 :max 4000 :step 1}
   obus OB
   ibus IB]
  (let [sig (in:ar ibus 1)
        c (+ 30 (* (in:kr cutoff 1) 40))] ;; the input is 0-100, the out put must be 0-4000
    (out obus (lpf sig c))))

(defsynth hp-filt
  [cutoff {:default B1 :def 1000 :min 0 :max 300 :step 1}
   obus OB
   ibus IB]
  (let [sig (in:ar ibus 1)
        c (* (in:kr cutoff 1) 3)]
    (out obus (hpf sig c))))

(defsynth bp-filt
  [freq {:default B1 :def 300 :min 100 :max 4000 :step 1}
   q {:default B2 :def 1 :min 1 :max 4 :step 1}
   obus OB
   ibus IB]
  (let [sig (in:ar ibus 1)
        f (+ 100 (* (in:kr freq 1) 40))
        qq (+ 1 (* (in:kr q 1) 0.04))]
    (out obus (bpf sig f qq))))

(defsynth moog-filt
  [cutoff {:default B1 :def 300 :min 0 :max 4000 :step 1}
   lpf-res {:default B2 :def 1 :min 0 :max 4 :step 1}
   obus OB
   ibus IB]
  (let [sig (in:ar ibus 1)
        c (* (in:kr cutoff 1) 40)
        l (* (in:kr lpf-res 1) 1)]
    (out obus (moog-ff sig c l))))

(defsynth mult
  [obus OB
   ibus IB
   gain  {:default B1 :def 50 :min 0 :max 100 :step :1}]
  (let [sig (in:kr ibus 1)]
    (out obus (* sig (in:kr gain 1)))))

(defsynth pct-add
  "50 = 0%, 0 = -50%, 100= +50%"
  [obus OB
   ibus IB
   gain  {:default B1 :def 50 :min 0 :max 100 :step :1}]
  (let [sig (in:kr ibus 1)
        g (+ (* (in:kr gain 1) 0.01) 0.5)]
    (out obus (* sig g))))

(defsynth val-add
  [obus OB
   ibus IB
   val  {:default B1 :def 0 :min 0 :max 100 :step :1}]
  (let [sig (in:kr ibus 1)]
    (out obus (+ sig (in:kr val 1)))))

;; (defsynth adsr-env
;;   [obus OB
;;    ibus IB
;;    attack  {:default B1 :def 0.1 :min 0 :max 100 :step 1}
;;    decay   {:default B2 :def 10 :min 0 :max 100 :step 1}
;;    sustain {:default B3 :def 60 :min 0 :max 100 :step 1}
;;    release {:default B4 :def 30 :min 0 :max 100 :step 1}]
;;   (let [sig (in:ar ibus 2)
;;         ;; a (* 0.01 (in:kr attack 1))
;;         ;; d (* 0.01 (in:kr decay 1))
;;         ;; s (* 0.01 (in:kr sustain 1))
;;         ;; r (* 0.01 (in:kr release 1))
;;         ;; gate 1
;;         ;; env (env-gen (adsr a d s r) :gate gate :action FREE)
;;         ]
;;     (out obus sig)))

(defsynth adsr-env
  [obus OB
   ibus IB
   attack  {:default B1 :def 0.1 :min 0 :max 100 :step 1}
   decay   {:default B2 :def 10 :min 0 :max 100 :step 1}
   sustain {:default B3 :def 60 :min 0 :max 100 :step 1}
   release {:default B4 :def 30 :min 0 :max 100 :step 1}
   gate 1
   ]
  (let [sig (in:ar ibus 1)
        a (* (in:kr attack 1) 0.01)
        d (* (in:kr decay 1) 0.01)
        s (* (in:kr sustain 1) 0.01)
        r (* (in:kr release 1) 0.01)
        env (env-gen (adsr a d s r) gate)
        ]
    (out obus (* env sig))))

(defsynth perc-env
  [obus OB
   ibus IB
   attack  {:default B1 :def 0.1 :min 0 :max 100 :step 1}
   release {:default B2 :def 30 :min 0 :max 100 :step 1}
   gate 1
   ]
  (let [sig (in:ar ibus 1)
        a (* (in:kr attack 1) 0.01)
        r (* (in:kr release 1) 0.01)
        env (env-gen (perc a r) gate)
        ]
    (out obus (* env sig))))

(defsynth amp
  "provides a gain of 4x at max (100)"
  [obus OB
   ibus IB
   gain  {:default B1 :def 20 :min 0 :max 100 :step :1}]
  (let [sig (in:ar ibus 1)
        g (* (in:kr gain 1) 0.1)]
    (out obus (* g sig))))

(defsynth freeverb
  [obus OB
   ibus IB
   wet-dry      {:default B1 :def 50 :min 0 :max 100 :step 1}
   room-size    {:default B2 :def 30 :min 0 :max 100 :step 1}
   dampening    {:default B3 :def 30 :min 0 :max 100 :step 1}
   ]
  (let [sig (in:ar ibus 1)
        w (/ (in:kr wet-dry 1) 100.0)
        r (/ (in:kr room-size 1) 100.0)
        d (/ (in:kr dampening 1) 100.0)]
    (out obus (free-verb sig w r d))))

(defsynth echo
  "max delay 5 seconds, so delay-time input 100 = 5 second delay"
  [obus OB
   ibus IB
   delay-time {:default B2 :def 20 :min 0 :max 100 :step 1}
   decay-time {:default B3 :def 20 :min 0 :max 100 :step 1}]
  (let [sig (in:ar ibus 1)
        max 5
        dlt (/ (in:kr delay-time 1) 20.0)
        dct (/ (in:kr decay-time 1) 20.0)
        echo (comb-n sig max dlt dct)]
    (out obus (+ echo sig))))

(defn bus-monitor-group [bs]
  (into {} (map #(let [[k v] %
                       bm (bus-monitor v)]
                   [k bm]) bs)))

(defn cbus [name] (control-bus 1 name))

(defn abus [name]  (audio-bus 1 name))

(defn connect-points
  ([n1 n2 ct c] (connect-points n1 n2 ct c "no-name" :obus))
  ([n1 n2 ct c name] (connect-points n1 n2 ct c name :obus))
  ([n1 n2 ct c name ob]
   (let [bus (if (= ct :audio) (abus name) (cbus name))]
     ;;(println "connect points: " bus n1 n2 ct c name ob)
     (ctl n1 ob bus)
     (ctl n2 c bus)
     bus)))

(defn connect-controls
 [n1 n2 c]
 (let [bus (control-bus 1)]
   (ctl n1 :obus bus)
   (ctl n2 c bus)
   bus))


(def sctl ctl)
(def svolume volume)
(def skill kill)

(defn print-test-monitors [b]
  (doseq [c b]
    (let [m (bus-monitor c)]
      (println (:name c) (deref m)))))

(defn s-test1 []
  (let [m (midi-in)
        s (square-osc)
        v (sin-vco)
        a (audio-out)]
    (connect-points m s :control :ibus "midi-in/out -> square-osc/in")
    (connect-points v s :control :width "sin-vco/out -> square-osc/width")
    (connect-points s a :audio :ibus "square-osc/out -> audio-out")
    (ctl v :freq 1)
    ))


(defsynth square-osc-0
  [obus OB
   note 60
   width 50]
  (let [freq (midicps note)]
    (out obus (pulse freq (/ width 100.0)))))

(defsynth square-osc-1
  [obus OB
   note 60]
  (let [freq (midicps note)
        width (* 100 (/ (+ 1 (sin-osc:kr 0.2)) 4))]
    (out obus (pulse freq (/ width 100.0)))))

;; this is an abuse-of-sorts of the parameter system for defsynths. For
;; modsynth synths, the default has to be a unique bus number. The other
;; parameters give the range of the values that this bus should receive.
(defsynth square-osc-3
  [obus OB
   ibus IB
   width {:default 2 :max 50 :min 10 :step 1}]
  (let [freq (in:kr ibus 1)
        w (in:kr width 1)
        c (const)]
    (out obus (pulse freq (/ w 100.0)))))

(defn s-test3 []
  (let [m (midi-in)
        s (square-osc-3)
        v (sin-vco)
        b1 (control-bus 2)
        b2 (control-bus 2)]
    (ctl m :obus b1)
    (ctl s :ibus b1)
    (ctl v :obus b2)
    (ctl s :width b2)
    (ctl v :freq 0.2)))

(defn s-test4 []
  (let [m (midi-in)
        s (square-osc)
        v (sin-vco)
        a (audio-out)
        c (const)]
    (ctl c :ibus 0.2)
    [(connect-points m s :control :ibus "midi-in -> square-osc:in")
     (connect-points v s :control :width "sin-vco -> square-osc:width")
     (connect-points c v :control :ibus "const -> sin-vco")
     (connect-points s a :audio :b1 "square-osc -> audio-out")]))

(defn s-test4a []
  (let [m (midi-in)
        s (saw-osc)
        a (audio-out)]
    [(connect-points m s :control :ibus "midi-in -> saw-osc")
     (connect-points s a :audio :b1 "saw-osc -> audio-out")]))

(defn s-test4b []
  (let [c1 (midi-in)
        c2 (midi-in)
        s1 (saw-osc)
        s2 (saw-osc)
        a (audio-out)]
    (ctl c1 :note 48)
    (ctl c2 :note 60)
    [(connect-points c1 s1 :control :ibus "midi-in -> saw-osc1")
     (connect-points c2 s2 :control :ibus "midi-in -> saw-osc2")
     (connect-points s1 a :audio :b1 "saw-osc -> audio-out1")
     (connect-points s2 a :audio :b2 "saw-osc -> audio-out2")]))

(defn s-test4c []
  (let [c1 (midi-in)
        sp (c-splitter)
        s1 (saw-osc)
        s2 (saw-osc)
        a (audio-out)]
    (ctl c1 :note 48)
    [(connect-points c1 sp :control :ibus "midi-in -> splitter")
     (connect-points sp s1 :control :ibus "midi-in -> saw-osc1" :ob1)
     (connect-points sp s2 :control :ibus "midi-in -> saw-osc2" :ob2)
     (connect-points s1 a :audio :b1 "saw-osc -> audio-out1")
     (connect-points s2 a :audio :b2 "saw-osc -> audio-out2")]))

(defn s-test5 []
  (let [m (midi-in)
        s (square-osc-3)
        c (const)
        a (audio-out)]
    (connect-points m s :control :ibus)
    (connect-points c s :control :width)
    (connect-points s a :audio :ibus)
    (ctl c :val 50)
    c))
