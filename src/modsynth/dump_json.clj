(ns modsynth.dump-json
  (:require [clj-json [core :as json]]))

(defn dump-file [inf outf]
  (let [data (load-file inf)
        js (json/generate-string data)
        mjs (clojure.string/replace (clojure.string/replace js ",{" ",\n{") ",[" ",\n[")]
    (spit outf mjs)))
