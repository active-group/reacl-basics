(ns ^:no-doc reacl-basics.utils
  (:require [clojure.spec.alpha :as s]))

(defn spec-parser [spec]
  (fn [args]
    (let [v (s/conform spec args)]
      (if (s/invalid? v)
        (throw (ex-info (s/explain-str spec args) (s/explain-data spec args)))
        v))))
