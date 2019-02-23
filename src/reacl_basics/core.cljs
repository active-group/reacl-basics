(ns reacl-basics.core
  (:require [reacl2.core :as reacl]))

(defn ^:no-doc attributes? [v] ;; TODO: should be public in reacl.
  (and (map? v)
       (not (satisfies? reacl/IHasDom v))))

(let [empty-opt (reacl/opt)]
  (defn ^:no-doc with-opt-detector [f]
    (fn [opt? & args]
      (if (instance? reacl/Options opt?)
        (apply f opt? args)
        (apply f empty-opt args)))))

(defn ^:no-doc attrs-detector [f & args]
  (if (and (not-empty args)
           (attributes? (first args)))
    (apply f args)
    (apply f {} args)))

