(ns reacl-basics.core
  (:require [reacl2.core :as reacl :include-macros true])
  (:refer-clojure :exclude [constantly]))

(defn ^:no-doc attributes? [v] ;; TODO: should be public in reacl.
  (and (map? v)
       (not (satisfies? reacl/IHasDom v))))

(let [empty-opt (reacl/opt)]
  (defn ^:no-doc with-opt-detector [f]
    (fn [opt? & args]
      (if (reacl/opt? opt?)
        (apply f opt? args)
        (apply f empty-opt args)))))

(defn ^:no-doc attrs-detector [f & args]
  (if (and (not-empty args)
           (attributes? (first args)))
    (apply f args)
    (apply f {} args)))

(reacl/defclass ^{:doc "A non-app-state class that always renders as `content` and does nothing else."
                  :arglists '([content])}
  constantly
  this [content]
  render content)

(defrecord ^:no-doc ActionReducer [f args]
  IFn
  (-invoke [this state action]
    (apply f state action args)))

(defn action-reducer
  "Returns a functions that takes `[app-state action]` as arguments, and then calls `f` on them and any additional `args`."
  [f & args]
  (ActionReducer. f args))

(defn reduce-action
  "Pipes all actions produced by the given component `elem`
  through `(f app-state action & args)`, where `app-state` is
  undefined if `elem` is an instance of a non-app-state class."
  [elem f & args] ;; TODO: I'm still not convinced that this should be possible; should take a class, and an action-reducer.
  (constantly (reacl/opt :reduce-action
                         (apply action-reducer f args))
              elem))
