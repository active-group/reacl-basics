(ns reacl-basics.subs.core
  (:require [reacl2.core :as reacl :include-macros true]
            [reacl-basics.actions.core :as actions])
  (:refer-clojure :rename {map cmap}
                  :exclude [map concat]))

(defprotocol ^:no-doc ISubscribable
  (subscribe-action [this target make-id-message make-value-message] "Returns an action, that must return a `(make-id-message id)` immediately, and may send one or more `(make-value-message value)` to target later.")
  (unsubscribe-action [this id] "Returns an action that will immediately cancel the subscription with the given id."))

(defn subscribable?
  "Returns true of the given value is a subscribable."
  [v]
  (satisfies? ISubscribable v))

;; TODO: with-subscription for continous data; something else possible for discrete data (-> a message)?

(defn- id-msg [id] [:id id])
(defn- val-msg [msg] [:value msg])

(reacl/defclass ^{:arglists '([sub f & args])
                  :doc "A non-app-state class that subscribes to the
  given subscribable `sub` as long as it's mounted, and and renders as `(f
  value & args)` where `value` is the most recent value from the
  subscription, or `nil` initially."}
  subscribe-to this [sub f & args]

  local-state [state {:id nil
                      :value nil ;; TODO: allow different defaults than nil?
                      }]
  component-did-mount
  (fn []
    (reacl/return :action (subscribe-action sub this id-msg val-msg)))

  render
  (apply f (:value state) args)
  
  component-will-unmount
  (fn []
    (if-let [id (:id state)]
      (reacl/return :action (unsubscribe-action sub id))
      (reacl/return)))

  handle-message
  (fn [msg]
    (case (first msg)
      :id (reacl/return :local-state (assoc state :id (second msg)))
      :value (reacl/return :local-state (assoc state :value (second msg))))))

(defrecord ^:no-doc Subscribable [make-unsub-action make-sub-action args]
  ISubscribable
  (subscribe-action [this target make-id-message make-value-message]
    (apply make-sub-action target make-id-message make-value-message args))
  (unsubscribe-action [this id]
    (apply make-unsub-action id args)))

(defn subscribable
  "Returns a subscribable based on the given functions that make
  subscribe and unsubscribe actions. Extra arguments are appended to
  both calls to those function."
  [make-unsub-action make-sub-action & args]
  (Subscribable. make-unsub-action make-sub-action args))

(letfn [(ss-sub [target make-id-message make-value-message make-unsub-action make-sub-action sub-args]
          (apply make-sub-action target make-id-message make-value-message sub-args))
        (ss-unsub [id make-unsub-action make-sub-action sub-args]
          (make-unsub-action id))]
  (defn simple
  "Returns a subscribable based on the given functions that make
  subscribe and unsubscribe actions. Extra arguments are appended only
  to the call to `make-sub-action`."
    [make-unsub-action make-sub-action & sub-args]
    (subscribable make-unsub-action make-sub-action sub-args)))

(letfn [(p-sub [target make-id-message make-value-message make-main-action]
          ;; TODO: should this return different, random ids?
          (actions/comp (actions/message target (make-id-message :id))
                        (make-main-action target make-value-message)))
        (p-unsub [id]
          actions/nothing)]
  (defn pure
    "Returns a subscribable that does not need side-effectful
  subscriptions, where `(make-main-action target make-value-message & args)` that does not need to send an
  `id` to target. Note that an id is sent anyway."
    [make-main-action & args]
    (simple p-unsub p-unsub make-main-action args)))

(def ^{:doc "A subscribable that never yields a value."}
  void
  (let [nothing (constantly actions/nothing)]
    (pure nothing)))

(letfn [(ms-sub [target make-id-message make-value-message sub f args]
          (subscribe-action sub target
                            make-id-message
                            (fn [value] ;; TODO: bind fn
                              (apply f value args))))
        (ms-unsub [id sub f args]
          (unsubscribe-action sub id))]
  (defn map
    "Returns a subscribable that yields the same values as `sub`, but piped through `(f value & args)`."
    [sub f & args]
    (subscribable ms-unsub ms-sub sub f args)))

(letfn [(c-mk [target make-value-message value]
          (actions/message target (make-value-message value)))]
  (defn const
    "Returns a subscribable that yields the given value immediately."
    [value]
    (pure value)))

;; TODO: needs return :message.
#_(defrecord ^:no-doc ConcatSubscribable [subs]
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
                 ;; TODO: check which messages are the id messages? There can be others.
                 (reacl/return :message [target (cmap second (core/returned-messages a))])))
             (unsubscribe [this id]
               (apply actions/comp-actions (cmap (fn [id s]
                                                   (unsubscribe s id))
                                                 id
                                                 subs))))

#_(defn concat
  "Returns a subscribable that yield the values of all the given subs in the same order."
  [& subs]
  (ConcatSubscribable. subs))

(letfn [(w-sub [target make-id-message make-value-message sub size]
          (let [prev (atom (repeat size nil))]
            (subscribe-action sub target make-id-message (fn [value] ;; TODO: bind fn.
                                                           (let [res (conj (vec (rest @prev)) value)]
                                                             (reset! prev res)
                                                             (make-value-message res))))))
        (w-unsub [id sub size]
          (unsubscribe-action sub id))]
  (defn sliding-window
    "Returns a subscribable on a vector of the most recent `size`
  values of `sub`. The newest value will be last. Initially the vector
  is padded with nils."
    [sub size]
    (subscribable w-unsub w-sub sub size)))
