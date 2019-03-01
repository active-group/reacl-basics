(ns reacl-basics.actions.core
  "Utilites to defines actions and utilities to specialize and execute
  them."
  (:require [reacl-basics.core :as core]
            [reacl2.core :as reacl])
  (:refer-clojure :exclude [comp]))

(defrecord ^:no-doc Action [f args]
  IFn
  (-invoke [this state] (apply f state args)))

(defn execute! [a state]
  (a state))

(defn action
  "Returns an action that contains the function to execute it as by `(f state & args) => (reacl/return ...))`"
  [f & args]
  (Action. f args))

(defn action?
  "Return true if `v` is an action created by [[action]]."
  [v]
  (instance? Action v))

(def ^{:doc "An action that does nothing."}
  nothing (action (constantly (reacl/return))))

(letfn [(right-state [stl str]
          (if (= str reacl/keep-state)
            stl
            str))
        (comp-a [state a1 a2]
          (cond
            :else
            (let [r1 (a1 state)
                  st2 (right-state state (reacl/returned-app-state r1))
                  r2 (a2 st2)]
              (reacl/concat-returned r1 r2))))
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
    (action state)
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
    (action ext-a f args)))

(letfn [(msg-a [state target f args]
          (let [allowed (atom false)
                send! (fn [msg]
                        (when-not @allowed
                          (throw (ex-info "Messages must only be emitted asynchronously by the send! function." {:message msg})))
                        (reacl/send-message! target msg))]
            (if-let [msg (try (apply f send! args)
                              (finally (reset! allowed true)))]
              (reacl/return :message [target msg])
              (reacl/return))))]
  (defn async-messages
    "Creates an action that sends messages to the component
  `target`. One message may be directly returned by `f`, which is
  delivered to `target` immediately, unless it's nil. More can be sent
  later with an `send!` function that is passed as the first
  argument to `f`."
    [target f & args]
    (action msg-a target f args)))

(letfn [(cnst [send! message]
          message)]
  (defn message
    "Creates an action that sends the given message to the component
  `target`."
    [target message]
    (async-messages target cnst message)))
