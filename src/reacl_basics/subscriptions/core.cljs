(ns reacl-basics.subscriptions.core
  (:require [reacl2.core :as core :include-macros true]
            )
  (:refer-clojure :exclude [delay]))

(defprotocol ISubscription
  (subscribe [this target make-id-message make-value-message] "Returns an action, that will send an id and later a value (possibly multiple times) to target.")
  (unsubscribe [this id] "Returns an action that will cancel the subscription with the given id."))

(defn- id-msg [id] [:id id])
(defn- val-msg [msg] [:msg id])

(reacl/defclass with-subscription this [sub f & args]
  local-state [state {:id nil
                      :value nil  ;; TODO: allow different defaults than nil?
                      }]
  component-did-mount
  (reacl/return :action (subscribe sub this id-msg val-msg))

  render
  (apply f (:value state) args)
  
  component-will-unmount
  (if-let [id (:id state)]
    (reacl/return :action (unsubscribe sub id))
    (reacl/return))

  handle-message
  (fn [msg]
    (case (first msg)
      :id (reacl/return :local-state (assoc local-state :id (second msg)))
      :value (reacl/return :local-state (assoc local-state :value (second msg))))))

(defrecord ^:no-doc SimpleSubscription [make-unsub-action make-sub-action sub-args]
  ISubscription
  (subscribe [this target make-id-message make-value-message]
    (make-sub-action target make-id-message make-value-message))
  (unsubscribe [this id]
    (make-unsub-action id)))

(defn simple-subscription [make-unsub-action make-sub-action & sub-args]
  (SimpleSubscription. make-unsub-action make-sub-action sub-args))
