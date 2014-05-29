(ns modsynth.midi
  "
At present only supports single note at a time instruments which is what I play. For keyboard support
some form of multi note will have to be implemented.
"
  (:use [overtone.studio.midi]
        [overtone.sc.node]
        [overtone.sc.dyn-vars])
  (:require [overtone.libs.event :as e]))

(def event-registry (atom {}))
(def last-note* (atom {:note* 48}))
(def profile {:device-key [:midi]
              :player-key ::yamaha-wx7
              :central-bend-point 78.0
              :bend-factor 0.1})

(defn register-note-events [synth]
  (swap! event-registry assoc-in [:note :synth] synth))

(defn register-discreet-cc-events
  "
wf is a write function that will be invoked for changes.
rf is a read function from which the current value can be obtained.
num is a compound number in which the hundreds represent the cc and the ones/tens
represent the maximum value the event can fire and yes this is extreme hackage
"
  [synth num wf rf]
  (let [cc-num (int (/ num 100))
        cc-max (rem num 100)]
    (println "register discreet cc num " cc-num " max val " cc-max)
    (swap! event-registry assoc-in [:cc cc-num] {:type :discreet :synth synth :write-fun wf :read-fun rf :max cc-max :current 0})))

(defn register-continuous-cc-events
  [synth num]
  (swap! event-registry assoc-in [:cc num] {:type :continuous :synth synth}))

(defn register-pc-events [synth num]
  (swap! event-registry assoc-in [:pc num] synth))

;; (defn control-vals [p amp]
;;   (let [s (:symbol p)
;;         val (+ (:offset p)
;;                (* amp (:range p)))]
;;     (fire-event s val)
;;     [s val]))

(defn control-vals [p amp]
  ["ibus" (* amp 100.0 )])

(defn discreet-change [p]
  (let [wf (:write-fun p)
        rf (:read-fun p)
        max (:max p)
        c (inc (rf))
        val (if (> c max) 0 c)]
    (wf val)
    ["ibus" val]))


(defn init []
  (let [device-key    (:device-key profile)
        player-key    (:player-key profile)
        central-bend-point (:central-bend-point profile)
        bend-factor   (:bend-factor profile)
        on-event-key  (concat device-key [:note-on])
        ;;off-event-key  (concat device-key [:note-off])
        pb-event-key  (concat device-key [:pitch-bend])
        cc-event-key  (concat device-key [:control-change])
        pc-event-key  (concat device-key [:program-change])
        on-key        (concat [player-key] on-event-key)
        pb-key        (concat [player-key] pb-event-key)
        cc-key        (concat [player-key] cc-event-key)
        pc-key        (concat [player-key] pc-event-key)]
    (reset! event-registry {})
    "note-on events send the new note to the synth and save that as the last-note played"
    (e/on-event
     on-event-key
     (fn [{note :note velocity :velocity}]
       (if-let [synth (get-in  @event-registry [:note :synth])]
         (let [amp (float (/ velocity 127))]
           (with-inactive-node-modification-error :silent
             (node-control synth [:note note :amp amp :velocity velocity]))
           (swap! last-note* assoc
                  :note* note))))
     on-key)

    "
TODO: off event isn't needed since the wx7 is at rest unless I'm blowing - this isn't the
case for keyboard and other input and I'll need to deal with that"

    ;; (e/on-event
    ;;  off-event-key
    ;;  (fn [{note :note velocity :velocity}]
    ;;    (if-let [synth (get-in @event-registry [:note :synth])]
    ;;      (with-inactive-node-modification-error :silent
    ;;        (node-control synth [:note note :amp 0 :velocity 0]))
    ;;      (swap! last-note* assoc
    ;;             :note* note)))
    ;;  on-key)

    "
pitch-bend events mutate the the note that the synth is playing. bend-factor
and central-bend-point are specified in a separate profile for the specific device.

For example, given a last note played of middle c note is 48. This number is modified by pitch
bend to produce the resultant note where,

new-note = last-note + bend-factor * (bend - central-bend-point)

last-note is not affected until a new note-on event occurs.
"
    (e/on-event
     pb-event-key
     (fn [{dummy :note bend :velocity}]
       (if-let [synth (get-in @event-registry [:note :synth])]
        (if-let [raw-note (:note* @last-note*)]
          (let [note (+ raw-note (* bend-factor (- bend central-bend-point)))]
            (with-inactive-node-modification-error :silent
              (node-control synth [:note note]))))))
     pb-key)

    "
cc events are either continuous type events e.g. foot pedal, or discreet type events e.g. foot switch.
Continuous events are relatively easier since the value is always what comes from the device.

Discreet events are given a max value. The output initializes outputting 0. Each click on the event increments the output
until the max value is exceeded at which point it goes back to 0. For a discreet on-off, then, one would set the max to 1.
"
    (e/on-event
     cc-event-key
     (fn [{cc :note velocity :velocity}]
       (println "cc n/v " cc velocity )
       (when-let [p (get-in @event-registry [:cc cc])]
         (try
           (let [amp (float (/ velocity 127))]
             (with-inactive-node-modification-error :silent
               (node-control (:synth p)
                             (case (:type p)
                               :continuous (control-vals p amp)
                               :discreet (discreet-change p)))))
           (catch Exception e (println "unexpected cc: " cc))))
       )
     cc-key)

    "
program change events can be used if necessary, but it would be best not to use them to control
the player. They should be reserved for specifying what synth is currently running at a higher level (see
program.clj)"
    ;; (e/on-event
    ;;  pc-event-key
    ;;  (fn [{program :note dummy :velocity}]
    ;;    (when-let [p (get pc-switches program)]
    ;;      (with-inactive-node-modification-error :silent
    ;;        (node-control synth (discreet-change p)))))
    ;;  pc-key)
    ))
