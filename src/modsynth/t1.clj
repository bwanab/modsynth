(ns modsynth.t1
  (:use overtone.live)  ; to work on gui without overtone comment out this line
  )

(def OB 2)
(def IB (+ OB 2))

(defsynth midi-in
  "midi-in synth"
  [obus OB
   note 48]
  (let [freq (midicps note)
        snd (sin-osc freq)]
    (out:ar obus snd)))

(defsynth perc-env
  "envelope synth"
  [obus OB
   ibus IB
   gate 1]
  (let [snd (in:ar ibus 2)
        env  (env-gen (perc 0.1 0.8) gate)]
    (out obus (* snd env))))

(defsynth adsr-env
  [obus OB
   ibus IB
   gate 1
   ]
  (let [sig (in:ar ibus 2)
        env (env-gen (adsr 0.1 0.1 0.6 0.3) gate)
        ]
    (out obus (* env sig))))

(defsynth aout
  "audio output synth"
  [obus 0
   ibus IB]
  (let [snd (in:ar ibus 2)]
    (out obus snd)))

(defn test1
  "test midi-in to envelope to audio out;
   returns a vector [midi-in perc-env]"
  []
  (let [mi (midi-in)
        pe (perc-env)
        ao (aout)
        b1 (audio-bus 2)
        b2 (audio-bus 2)]
    (ctl mi :obus b1)
    (ctl pe :ibus b1)
    (ctl pe :obus b2)
    (ctl ao :ibus b2)
    [mi pe]))

(defn test2
  "test midi-in to envelope to audio out;
   returns a vector [midi-in perc-env]"
  []
  (let [mi (midi-in)
        ae (adsr-env)
        ao (aout)
        b1 (audio-bus 2)
        b2 (audio-bus 2)]
    (ctl mi :obus b1)
    (ctl ae :ibus b1)
    (ctl ae :obus b2)
    (ctl ao :ibus b2)
    [mi ae]))


(defn scheduled-test [curr-t sep-t m note delay]
  (let [new-t (+ curr-t sep-t)
        mi (first m)
        pe (second m)]
    (ctl mi :note note)
    (ctl pe :gate 0)
    (at (+ (now) delay) (ctl pe :gate 1))
    (apply-at new-t #'scheduled-test [new-t sep-t m (inc note) delay])))

;; (def m (test2))
;; (scheduled-test (now) 1000 m 56 50)

(definst ding
  [note 60 velocity 1 gate 1]
  (let [freq (midicps note)
        snd  (sin-osc freq)
        env  (env-gen (perc 0.1 0.8) gate)]
    (* velocity env snd)))

(defn scheduled-test-1 [curr-t sep-t m note delay]
  (let [new-t (+ curr-t sep-t)]
    (ctl m :note note)
    (ctl m :gate 0)
    (at (+ (now) delay) (ctl m :gate 1))
    (apply-at new-t #'scheduled-test-1 [new-t sep-t m (inc note) delay])))




;; (defn test-runner [t]
;;   (do
;;     (println "start " (if t "test0" "test1"))
;;     (let [m1 (if t (test0) (test1))]
;;       (scheduled-test (now) 1000 m1 56))))

;(apply-at (now) #'test-runner [true])
;(apply-at (+ (now) 15000) #'test-runner [false])


;; when start test0 prints you get a series of hums as expected
;; when start test1 prints you get a single ding with expected envelop, but no rising dings - why?

;; these tests can simply be replicated at repl:

;; (def m0 (test0))
;; (scheduled-test (now) 1000 m0 56)
;; (def m1 (test1))
;; (scheduled-test (now) 1000 m1 56)

(defsynth pantest2
  [note 60
   pos 0]
  (let [freq (midicps note)
        osc (saw freq)
        p (pan2 osc pos)]
    (out [0 1] p)))

(defonce index-buffer
  (let [buf (buffer 128)
        data (take-while #(< % 100) (drop-while #(> % 30) (scale-field :a :pentatonic)))]
    (doseq [[idx val] (map-indexed (fn [a b] [a b]) data)]
      (buffer-set! buf idx val))
    buf))

(defsynth randtest
  [cycle-freq 5
   gate 1]
  (let [vco (sin-osc:kr cycle-freq)
        idx (t-rand:kr 10 25 vco)
        note (index:kr index-buffer idx)
        freq (midicps note)
        sig (saw freq)
        env (env-gen (adsr 1 10 10 10) gate)]
    (do
      (poll vco note "note: ")
      (out 0 (* env sig)))))

(defsynth sin-vco
  "normalizes to output a sine wave that goes from 0 to 100"
  [obus OB
   cycle-freq 1]
  (out obus (sin-osc:kr cycle-freq)))

(defsynth randtest2
  [ibus IB]
  (let [vco (in:kr ibus 1)
        note (t-rand:kr 40 60 vco)
        freq (midicps note)]
    (do
      (poll vco note "note: ")
      (out 0 (saw freq)))))

(defn test3
  "test t-rang getting trigger via bus"
  []
  (let [sv (sin-vco)
        rn (randtest2)
        b1 (control-bus 1)]
    (ctl sv :obus b1)
    (ctl rn :ibus b1)))
