(ns reacl-basics.core
  (:require [reacl2.core :as reacl])
  (:refer-clojure :exclude [identity]))

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

(reacl/defclass ^{:doc "A non-app-state class that does nothing but render as `content`."}
  identity
  this [content]
  render content)

(defrecord ^:no-doc ActionReducer [f args]
  IFn
  (-invoke [this state action]
    (apply f state action args)))

(defn action-reducer [f & args]
  (ActionReducer. f args))

(defn reduce-action [elem f & args]
  (identity (reacl/opt :reduce-action
                       (apply action-reducer f args))
            elem))
