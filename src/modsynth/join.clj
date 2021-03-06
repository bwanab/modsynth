(ns modsynth.join)

(defn find-common
  [s1 s2]
  (for [t1 s1 t2 s2 :when (= ;; (defn test-modsynth []
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
t1 t2)] t1))

(defn longer-first
  [s1 s2]
  (if (> (count s2) (count s1))
    [s2 s1]
    [s1 s2]))

(defn join-on-common
  "takes two sequences that possibly have common elements and returns a sequence
   in which the result is a sequence such that the elements of both are all included
   in the same order as in the original sequences, but all elements in each sequence
   that appear before the common elements are before the common elements in the result.
   The longer chain is selected to appear first in each joining.

   For example, the two sequences [:a :i :b :c :d :e] and [:f :b :g :d :h] will have the
   result [:a :i :f :b :c :g :d :e :h]"
  ([s1 s2] (join-on-common s1 s2 identity))
  ([s1 s2 f]
     (let [common (find-common s1 s2)]
       (loop [cc common t1 s1 t2 s2 acc []]
         (do
           (if (empty? cc) (concat acc t1 t2)
               (let [c (first cc)
                     p1 (split-with #(not= (f c) (f %)) t1)
                     p2 (split-with #(not= (f c) (f %)) t2)
                     [l1 l2] (longer-first (first p1) (first p2))]
                 (recur (rest cc) (rest (second p1)) (rest (second p2)) (concat acc l1 l2 [c])))))))))

(defn test-join []
  (let [s1 [:a :i :b :c :d :e]
        s2 [:f :b :g :d :h]
        res (join-on-common s1 s2)]
    (println res)
    (= res [:a :i :f :b :c :g :d :e :h])))

(defn test-join-2 []
  (let [s1 [:a :i :b :c :d :e :h]
        s2 [:f :b :g :d :h]
        res (join-on-common s1 s2)]
    (println res)
    (= res [:a :i :f :b :c :g :d :e :h])))
