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

(defn- return-tuples [& l]
  (apply reacl/return (apply concat l)))

(defn- returned-tuples [r]
  ;; FIXME: reacl should expose something for this:
  (assert (instace? reacl/Effects r))
  (let [v (vec (:args r))]
    (assert (even? (count v)))
    (map (fn [i]
           [(get prev i) (get prev (inc i))])
         (map #(* 2 %)
              (range (/ (count prev) 2))))))

(defn- returned-state [r st]
  (or (second (first (filter #(= st (first %))
                             (returned-tuples r))))
      reacl/keep-state))

(defn returned-app-state [r]
  (returned-state r :app-state))

(defn returned-local-state [r]
  (returned-state r :local-state))

(defn returned-actions [r]
  (map second (filter #(= :action (first %))
                      (returned-tuples r))))

(defn- set-returned-state [r st v]
  (return-tuples (cons [st v]
                       (remove #(= st (first %))
                               (returned-tuples r)))))

(defn set-returned-app-state [r v]
  (set-returned-state r :app-state v))

(defn set-returned-local-state [r v]
  (set-returned-state r :local-state v))

(defn add-returned-action [r action]
  (return-tuples (concat (returned-tuples r)
                         [:action action])))
