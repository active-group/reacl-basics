(ns reacl-basics.actions.core
  "Utilites to defines actions and utilities to specialize and execute
  them."
  (:require [reacl-basics.core :as core]
            [reacl2.core :as reacl])
  (:refer-clojure :exclude [comp delay map]))

(defprotocol IActionable
  (execute! [this state] "Execute the effect of the action and must return a 'reacl/return' value."))

(defrecord ^:no-doc Action [f args]
  IActionable
  (execute! [this state] (apply f state args)))

(defn action
  "Returns an action that contains the function to execute it as by `(f state & args) => (reacl/return ...))`"
  [f & args]
  (Action. f args))

(letfn [(gual [state f args]
          (execute! (apply f args) state))]
  (defn guard
    "Returns an action that, when executed, calls `(f & args)` which
  must return another action which is then executed immediately
  instead."
    [f & args]
    (action gual f args)))

(defn action?
  "Return true if `v` is an action created by [[action]]."
  [v]
  (satisfies? IActionable v))

(def ^{:doc "An action that does nothing."}
  nothing (action (constantly (reacl/return))))

(letfn [(right-state [stl str]
          (if (= str reacl/keep-state)
            stl
            str))
        (comp-a [state a1 a2]
          (let [r1 (execute! a1 state)
                st2 (right-state state (reacl/returned-app-state r1))
                r2 (execute! a2 st2)]
            (reacl/concat-returned r1 r2)))
        (comp-a_ [a1 a2]
          (cond
            (= a1 nothing) a2
            (= a2 nothing) a1
            :else (action comp-a a1 a2)))]
  (defn comp
    "Returns the sequential composition of executing all the given actions from left to right"
    [& actions]
    (reduce comp-a_
            nothing
            actions)))

(defn ^:no-doc handle-actions [state action]
  (if (action? action)
    (execute! action state)
    (reacl/return :action action)))

(defn action-handler
  "Wrapper around an element, that handles all [[actions?]] and lets
  any other actions through. Use this near the top level in your
  application to support the exection of the actions in this library."
  [content]
  (core/reduce-action content
                      handle-actions))

(letfn [(ext-a [state f args]
          (apply f args)
          (reacl/return))]
  (defn external
    "Creates an action that is executed solely by the side effects of
  applying `f` to `args`, i.e. is has an external effect that does not
  depend or modify an application state."
    [f & args]
    (assert (ifn? f) f)
    (action ext-a f args)))

(letfn [(msg-a [state target f args]
          (let [send! (fn [msg]
                        (reacl/send-message! target msg))]
            (let [msg (apply f send! args)]
              (if (some? msg)
                (reacl/return :message [target msg])
                (reacl/return)))))]
  (defn async-messages
    "Creates an action that sends messages to the component
  `target`. One message may be directly returned by `f`, which is
  delivered to `target` immediately, unless it's nil. More can be sent
  later with an `send!` function that is passed as the first
  argument to `f`."
    [target f & args]
    ;; TODO (assert (component? target) target)
    (assert (ifn? f) f)
    (action msg-a target f args)))

(letfn [(cnst [send! message]
          message)]
  (defn message
    "Creates an action that sends the given message to the component
  `target`."
    [target message]
    ;; TODO (assert (component? target) target)
    (async-messages target cnst message)))
