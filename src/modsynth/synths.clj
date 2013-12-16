(ns modsynth.synths
  (:use overtone.live)  ; to work on gui without overtone comment out this line
  )

;;(defmacro mod-defsynth [name p body] (let [sym-name (symbol (eval name))] `(o/defsynth ~sym-name ~p ~body)))
;;(defmacro mod-ctl [name t val] (let [sym-name (symbol (eval name))] `(o/ctl ~sym-name ~t ~val)))

;; to work on gui without overtone uncomment these lines
;; ******
;; (defmacro defsynth [name p body] `(defn ~name [] (comment ~p) (comment ~body)))
;; (defn control-bus [x])
;; (defn audio-bus [x])
;; (defn ctl [x y z])
;; *******


(defsynth midi-in
  [obus 0
   note {:default 60 :min 0 :max 120 :step 1}]
  (let [freq (midicps note)]
    (out:kr obus freq)))

(defsynth saw-osc
  [obus 0
   ibus 1]
  (let [freq (in:kr ibus 2)]
    (out obus (saw freq))))

(defsynth s_sin-osc
  [obus 0
   ibus 1]
  (let [freq (in:kr ibus 2)]
    (out obus (sin-osc freq))))

(defsynth square-osc
  [obus 0
   ibus 1
   width {:default 50 :min 5 :max 50 :step 1}]
  (let [freq (in:kr ibus 2)]
    (out obus (pulse freq (/ width 100.0)))))

(defsynth lp-filt
  [cutoff {:default 1000 :min 0 :max 4000 :step 1}
   obus 0
   ibus 1]
  (let [sig (in:ar ibus 2)]
    (out obus (lpf sig cutoff))))

(defsynth amp
  [obus 0
   ibus 1
   gain  {:default 30 :min 0 :max 100 :step :1}]
  (let [sig (in:ar ibus 2)]
    (out obus (* (/ gain 100.0) sig))))

(defn cbus [] (control-bus 2))

(defn abus []  (audio-bus 2))

(defn connect-nodes [n1 n2 ct]
  (let [bus (if (= ct :audio) (abus) (cbus))]
    (ctl n1 :obus bus)
    (ctl n2 :ibus bus)
    bus))

(def sctl ctl)
(def svolume volume)
