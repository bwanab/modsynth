(ns cljivtk.join)

(defn find-common
  [s1 s2]
  (for [t1 s1 t2 s2 :when (= t1 t2)] t1))

(defn longer-first
  [s1 s2]
  (if (> (count s2) (count s1))
    [s2 s1]
    [s1 s2]))

(defn join-on-common
  [s1 s2]
  (let [common (find-common s1 s2)]
    (loop [cc common t1 s1 t2 s2 acc []]
      (if (empty? cc) (concat acc (rest t1) (rest t2))
          (let [c (first cc)
                p1 (split-with #(not= c %) t1)
                p2 (split-with #(not= c %) t2)
                [l1 l2] (longer-first (first p1) (first p2))]
            (recur (rest cc) (second p1) (second p2) (concat l1 l2 [c])))))))
