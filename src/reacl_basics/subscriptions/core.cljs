(ns reacl-basics.subscriptions.core
  (:require [reacl2.core :as core :include-macros true]
            [reacl-basics.actions.core :as actions]))

(defprotocol ^:no-doc ISubscription
  (subscribe [this target make-id-message id-args make-value-message value-args] "Returns an action, that must return a `(make-id-message id & args)` immediately, and may send one or more `(make-value-message value & value-args)` to target later.")
  (unsubscribe [this id] "Returns an action that will immediately cancel the subscription with the given id."))

(defn subscription? [v]
  (satisfies? ISubscription v))

;; TODO: with-subscription for continous data; something else possible for discrete data (-> a message)?

(defn- id-msg [id] [:id id])
(defn- val-msg [msg] [:msg id])

(reacl/defclass with-subscription this [sub f & args]
  local-state [state {:id nil
                      :value nil  ;; TODO: allow different defaults than nil?
                      }]
  component-did-mount
  (reacl/return :action (subscribe sub this id-msg nil val-msg nil))

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

(defrecord ^:no-doc Subscription [make-unsub-action make-sub-action args]
  ISubscription
  (subscribe [this target make-id-message id-args make-value-message value-args]
    (apply make-sub-action target make-id-message id-args make-value-message value-args args))
  (unsubscribe [this id]
    (apply make-unsub-action id args)))

(defn subscription [make-unsub-action make-sub-action & args]
  (Subscription. make-unsub-action make-sub-action args))

(letfn [(ss-sub [target make-id-message id-args make-value-message value-args make-unsub-action make-sub-action sub-args]
          (apply make-sub-action target make-id-message id-args make-value-message value-args sub-args))
        (ss-unsub [id make-unsub-action make-sub-action sub-args]
          (make-unsub-action id))]
  (defn simple-subscription [make-unsub-action make-sub-action & sub-args]
    (subscription make-unsub-action make-sub-action sub-args)))

(def ^{:doc "A subscription that never yields a value."}
  void
  (simple-subscription (constantly actions/nothing)
                       (fn [target make-id-message id-args make-value-message value-args]
                         (actions/single-message-action target (apply make-id-message :id id-args)))))

(letfn [(mk-value-msg [value make-value-message f args value-args]
          (apply make-value-message (apply f value args) value-args))
        (ms-sub [target make-id-message id-args make-value-message value-args sub f args]
          (subscribe sub target
                     make-id-message id-args
                     mk-value-msg [make-value-message f args value-args]))
        (ms-unsub [id sub f args]
          (unsubscribe sub))]
  (defn map-subscription
    "Returns a subscription that yield the same values as `sub`, piped through `(f value & args)`."
    [sub f & args]
    (subscription ms-unsub ms-sub sub f args)))

;; TODO: needs return :message.
#_(defrecord ^:no-doc ConcatSubscription [subs]
  (subscribe [this target make-id-message id-args make-value-message value-args]
    (let [value (atom (mapv (constantly nil)
                            subs))
          a (apply actions/comp-actions (map-indexed (fn [idx s]
                                               (subscribe s target identity nil
                                                          (fn [v] ;; TODO: static
                                                            ;; update value atom, an send the current vector instead.
                                                            (reset! value (assoc @value idx v))
                                                            @value)
                                                          nil))
                                             subs))]
      ;; TODO: check which messages are the id messages? (need allow others?)
      (reacl/return :message [target (map second (core/returned-messages a))])))
  (unsubscribe [this id]
    (apply actions/comp-actions (map (fn [id s]
                                       (unsubscribe s id))
                                     id
                                     subs))))

#_(defn concat-subscriptions
  "Returns a subscription that yield the values of all the given subs in the same order."
  [& subs]
  (ConcatSubscription. subs))

