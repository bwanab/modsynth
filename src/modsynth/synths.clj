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
(def IB (+ OB 2))
(def B1 (+ IB 2))
(def B2 (inc B1))
(def B3 (inc B2))
(def B4 (inc B3))

(defsynth audio-out
  [obus 0
   ibus IB]
  (out obus (in:ar ibus 1)))

(defsynth const
  [obus OB
   ibus 0]
  (out obus (* ibus 1.0)))

(defsynth midi-in
  [obus OB
   note 60]
  (let [freq (midicps note)]
    (out:kr obus freq)))

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
  [cutoff {:default B1 :def 300 :min 0 :max 4000 :step 1}
   obus OB
   ibus IB]
  (let [sig (in:ar ibus 2)
        c (* (in:kr cutoff 1) 40)]  ;; the input is 0-100, the out put must be 0-4000
    (out obus (lpf sig c))))

(defsynth hp-filt
  [cutoff {:default B1 :def 1000 :min 0 :max 300 :step 1}
   obus OB
   ibus IB]
  (let [sig (in:ar ibus 2)
        c (* (in:kr cutoff 1) 3)]
    (out obus (hpf sig c))))

(defsynth bp-filt
  [freq {:default B1 :def 300 :min 100 :max 4000 :step 1}
   q {:default B2 :def 1 :min 1 :max 4 :step 1}
   obus OB
   ibus IB]
  (let [sig (in:ar ibus 2)
        f (+ 100 (* (in:kr freq 1) 40))
        qq (+ 1 (* (in:kr q 1) 0.04))]
    (out obus (bpf sig f qq))))

(defsynth moog-filt
  [cutoff {:default B1 :def 300 :min 0 :max 4000 :step 1}
   lpf-res {:default B2 :def 50 :min 0 :max 100 :step 1}
   obus OB
   ibus IB]
  (let [sig (in:ar ibus 2)
        c (* (in:kr cutoff 1) 40)
        l (* (in:kr lpf-res 1) 0.01)]
    (out obus (moog-ff sig c l))))

(defsynth amp
  [obus OB
   ibus IB
   gain  {:default B1 :def 20 :min 0 :max 100 :step :1}]
  (let [sig (in:ar ibus 2)
        g (* (in:kr gain 1) 0.01)]
    (out obus (* g sig))))

(defsynth freeverb
  [obus OB
   ibus IB
   wet-dry      {:default B1 :def 50 :min 0 :max 100 :step 1}
   room-size    {:default B2 :def 30 :min 0 :max 100 :step 1}
   dampening    {:default B3 :def 30 :min 0 :max 100 :step 1}
   ]
  (let [sig (in:ar ibus 2)
        w (/ (in:kr wet-dry 1) 100.0)
        r (/ (in:kr room-size 1) 100.0)
        d (/ (in:kr dampening 1) 100.0)]
    (out obus (free-verb sig w r d))))

(defsynth echo
  [obus OB
   ibus IB
   max-delay {:default B1 :def 20 :min 0 :max 500 :step 1}
   delay-time {:default B2 :def 20 :min 0 :max 100 :step 1}
   decay-time {:default B3 :def 20 :min 0 :max 100 :step 1}]
  (let [sig (in:ar ibus 2)
        m   (/ (in:kr max-delay 1) 100.0)
        dlt (/ (in:kr delay-time 1) 100.0)
        dct (/ (in:kr decay-time 1) 100.0)
        echo (comb-n sig m dlt dct)]
    (out obus (+ echo sig))))

(defn bus-monitor-group [bs]
  (apply hash-map
   (flatten (map #(let [[k v] %
                        bm (bus-monitor v)]
                    [k bm]) bs))))

(defn cbus [name] (control-bus 1 name))

(defn abus [name]  (audio-bus 2 name))

(defn connect-points [n1 n2 ct c & name]
  (let [bname (if name (first name) "no-name")
        bus (if (= ct :audio) (abus bname) (cbus bname))]
    (println "connect points: " bus n1 n2 ct c bname)
    (ctl n1 :obus bus)
    (ctl n2 c bus)
    bus))

(defn connect-controls
 [n1 n2 c]
 (let [bus (control-bus 1)]
   (ctl n1 :obus bus)
   (ctl n2 c bus)
   bus))


(def sctl ctl)
(def svolume volume)

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
    (connect-points m s :control :ibus)
    (connect-points v s :control :width)
    (connect-points c v :control :ibus)
    (connect-points s a :audio :ibus)
    (ctl c :val 0.2)))

(defn s-test4a []
  (let [m (midi-in)
        s (saw-osc)
        a (audio-out)]
    [(connect-points m s :control :ibus "midi-in -> saw-osc")
     (connect-points s a :audio :ibus "saw-osc -> audio-out")]))

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
