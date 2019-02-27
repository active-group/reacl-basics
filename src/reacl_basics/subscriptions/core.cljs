(ns reacl-basics.subscriptions.core
  (:require [reacl2.core :as reacl :include-macros true]
            [reacl-basics.actions.core :as actions]))

(defprotocol ^:no-doc ISubscription
  (subscribe [this target make-id-message make-value-message] "Returns an action, that must return a `(make-id-message id)` immediately, and may send one or more `(make-value-message value)` to target later.")
  (unsubscribe [this id] "Returns an action that will immediately cancel the subscription with the given id."))

(defn subscription? [v]
  (satisfies? ISubscription v))

;; TODO: with-subscription for continous data; something else possible for discrete data (-> a message)?

(defn- id-msg [id] [:id id])
(defn- val-msg [msg] [:value msg])

(reacl/defclass with-subscription this [sub f & args]
  local-state [state {:id nil
                      :value nil ;; TODO: allow different defaults than nil?
                      }]
  component-did-mount
  (fn []
    (reacl/return :action (subscribe sub this id-msg val-msg)))

  render
  (apply f (:value state) args)
  
  component-will-unmount
  (fn []
    (if-let [id (:id state)]
      (reacl/return :action (unsubscribe sub id))
      (reacl/return)))

  handle-message
  (fn [msg]
    (case (first msg)
      :id (reacl/return :local-state (assoc state :id (second msg)))
      :value (reacl/return :local-state (assoc state :value (second msg))))))

(defrecord ^:no-doc Subscription [make-unsub-action make-sub-action args]
  ISubscription
  (subscribe [this target make-id-message make-value-message]
    (apply make-sub-action target make-id-message make-value-message args))
  (unsubscribe [this id]
    (apply make-unsub-action id args)))

(defn subscription [make-unsub-action make-sub-action & args]
  (Subscription. make-unsub-action make-sub-action args))

(letfn [(ss-sub [target make-id-message make-value-message make-unsub-action make-sub-action sub-args]
          (apply make-sub-action target make-id-message make-value-message sub-args))
        (ss-unsub [id make-unsub-action make-sub-action sub-args]
          (make-unsub-action id))]
  (defn simple-subscription [make-unsub-action make-sub-action & sub-args]
    (subscription make-unsub-action make-sub-action sub-args)))

(def ^{:doc "A subscription that never yields a value."}
  void
  (simple-subscription (constantly actions/nothing)
                       (fn [target make-id-message make-value-message]
                         (actions/single-message-action target (make-id-message :id)))))

(letfn [(ms-sub [target make-id-message make-value-message sub f args]
          (subscribe sub target
                     make-id-message
                     (fn [value] ;; TODO: bind fn
                       (apply f value args))))
        (ms-unsub [id sub f args]
          (unsubscribe sub id))]
  (defn map-subscription
    "Returns a subscription that yield the same values as `sub`, piped through `(f value & args)`."
    [sub f & args]
    (subscription ms-unsub ms-sub sub f args)))

;; TODO: needs return :message.
#_(defrecord ^:no-doc ConcatSubscription [subs]
  (subscribe [this target make-id-message make-value-message]
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

(letfn [(w-sub [target make-id-message make-value-message sub size]
          (let [prev (atom (repeat size nil))]
            (subscribe sub target make-id-message (fn [value] ;; TODO: bind fn.
                                                    (let [res (conj (vec (rest @prev)) value)]
                                                      (reset! prev res)
                                                      (make-value-message res))))))
        (w-unsub [id sub size]
          (unsubscribe sub id))]
  (defn window-subscription
    "Returns a subscription on a vector of the most recent `size` values of `sub`. The newest value will be last."
    [sub size]
    (subscription w-unsub w-sub sub size)))
