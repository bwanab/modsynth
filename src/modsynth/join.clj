(ns modsynth.join)

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
      (do
        (println t1 t2 acc)
        (if (empty? cc) (concat acc t1 t2)
           (let [c (first cc)
                 p1 (split-with #(not= c %) t1)
                 p2 (split-with #(not= c %) t2)
                 [l1 l2] (longer-first (first p1) (first p2))]
             (recur (rest cc) (rest (second p1)) (rest (second p2)) (concat acc l1 l2 [c]))))))))

(defn test []
  (let [s1 [:a :b :c :d :e]
        s2 [:f :b :g :d :h]
        res (join-on-common s1 s2)]
    (println res)
    (= res [:a :f :b :c :g :d :e :h])))
