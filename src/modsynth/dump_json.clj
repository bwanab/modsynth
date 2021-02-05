(ns modsynth.dump-json
  (:require [clj-json [core :as json]]
            [clojure.java.io :as io]
            [clojure.string :as string]))


(defn dump-file [inf outf]
  (let [mjs
        (-> inf
            (load-file)
            (json/generate-string)
            (string/replace ":[" ":\n[")
            (string/replace ":{" ":\n{")
            (string/replace ",{" ",\n{")
            (string/replace "]," "],\n")
            )]
    (spit outf mjs)))

(defn dump-to [fname ind outd]
  (let [inf (string/join "/" [ind fname])
        outf (string/join "/" [outd (string/replace fname ".oms" ".json")])]
    (println inf outf)
    [inf outf]))

(defn mymap [coll f]
  (map f coll))

(defn myapply [coll f]
  (apply f coll))

(defn dump-all-files [ind outd]
  (-> ind
      (io/file)
      (.list)
      (mymap #(-> %
                  (dump-to ind outd)
                  (myapply dump-file)))))
