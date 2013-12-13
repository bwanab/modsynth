(ns modsynth.synths
  ;(:use overtone.live)  ; to work on gui without overtone comment out this line
  )

;;(defmacro mod-defsynth [name p body] (let [sym-name (symbol (eval name))] `(o/defsynth ~sym-name ~p ~body)))
;;(defmacro mod-ctl [name t val] (let [sym-name (symbol (eval name))] `(o/ctl ~sym-name ~t ~val)))

; to work on gui without overtone uncomment these lines until ******
(defmacro defsynth [name p body] `(defn ~name [] (comment ~p) (comment ~body)))
(defn control-bus [x])
(defn audio-bus [x])
(defn ctl [x y z])
; *******

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

(defsynth lp-filt
  [obus 0
   ibus 1]
  (let [sig (in:ar ibus 2)]
    (out obus (lpf sig 1000))))

(defsynth amp
  [obus 0
   ibus 1]
  (let [sig (in:ar ibus 2)]
    (out obus (* 0.3 sig))))

(defn cbus [] (control-bus 2))

(defn abus []  (audio-bus 2))

(defn connect-nodes [n1 n2 ct]
  (let [bus (if (= ct :audio) (abus) (cbus))]
    (ctl n1 :obus bus)
    (ctl n2 :ibus bus)
    bus))

(def sctl ctl)
