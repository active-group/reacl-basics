(ns reacl-basics.subs.core
  (:require [reacl2.core :as reacl :include-macros true]
            [reacl-basics.actions.core :as actions])
  (:refer-clojure :rename {map cmap}
                  :exclude [map concat]))

(defprotocol ^:no-doc ISubscribable
  (subscribe-action [this target make-id-message make-value-message] "Returns an action, that must return a `(make-id-message id)` to target immediately, and may send one or more `(make-value-message value)` to target later.")
  (unsubscribe-action [this id] "Returns an action that will immediately cancel the subscription with the given id."))

(defn subscribable?
  "Returns true if the given value is a subscribable."
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

  should-component-update?
  (fn [next-app-state next-local-state next-sub next-f & next-args]
    ;; if only the id changed, then don't update:
    (if (and (= [next-sub next-f next-args]
                [sub f args])
             (= (:value state) (:value next-local-state))
             (not= (:id state) (:id next-local-state)))
      false
      true))

  ;; FIXME: did-update ...when different sub, then restart.
  
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
      :value (reacl/return :local-state (assoc state :value (second msg)))
      (do (assert false (str "Unexpected msg: " (pr-str msg)))
          (reacl/return)))))

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
          (apply make-sub-action target make-id-message make-value-message
                 sub-args))
        (ss-unsub [id make-unsub-action make-sub-action sub-args]
          (make-unsub-action id))]
  (defn simple
  "Returns a subscribable based on the given functions that make
  subscribe and unsubscribe actions. Extra arguments are appended only
  to the call to `make-sub-action`."
    [make-unsub-action make-sub-action & sub-args]
    (assert (ifn? make-unsub-action))
    (assert (ifn? make-sub-action))
    (subscribable ss-unsub ss-sub make-unsub-action make-sub-action sub-args)))

(letfn [(p-sub [target make-id-message make-value-message make-main-action args]
          (actions/comp (actions/message target (make-id-message :id))
                        (apply make-main-action target make-value-message args)))
        (p-unsub [id]
          actions/nothing)]
  (defn pure
    "Returns a subscribable that does not need side-effectful
  subscriptions, where `(make-main-action target make-value-message & args)` does not need to send an
  `id` to target. Note that an id is sent anyway."
    [make-main-action & args]
    (assert (ifn? make-main-action))
    (simple p-unsub p-sub make-main-action args)))

(def ^{:doc "A subscribable that never yields a value."}
  void
  (let [nothing (constantly actions/nothing)]
    (pure nothing)))

(letfn [(ms-sub [target make-id-message make-value-message sub f args]
          (subscribe-action sub target
                            make-id-message
                            ;; TODO: bind fn
                            (fn [value]
                              (make-value-message (apply f value args)))))
        (ms-unsub [id sub f args]
          (unsubscribe-action sub id))]
  (defn map
    "Returns a subscribable that yields the same values as `sub`, but piped through `(f value & args)`."
    [sub f & args]
    (assert (subscribable? sub))
    (assert (ifn? f))
    (subscribable ms-unsub ms-sub sub f args)))

(letfn [(c-mk [target make-value-message value]
          (actions/message target (make-value-message value)))]
  (defn const
    "Returns a subscribable that yields the given value immediately."
    [value]
    (pure c-mk value)))

(letfn [(aa-sub [target make-id-message make-value-message subs]
          (actions/guard
           (fn []
             (let [ids (atom (vec (repeat (count subs) nil)))]
               (apply actions/comp
                      (actions/message target (make-id-message ids))
                      (map-indexed (fn [idx sub]
                                     (subscribe-action sub target
                                                       (fn [id]
                                                         (reset! ids (assoc @ids idx id))
                                                         nil)
                                                       (fn [value]
                                                         (make-value-message [idx value]))))
                                   subs))))))
        (aa-unsub [ids subs]
          (assert (= (count ids) (count subs)))
          (apply actions/comp (cmap (fn [sub id]
                                      (when (some? id) ;; the sub didn't send an id?
                                        (unsubscribe-action sub id)))
                                    subs ids)))]
  (defn indexed
    "Returns a subscribable that yields the values of all of the given
  subs as they come in, as a tuple `[idx value]` with the position of
  the corresponding sub."
    [& subs]
    (if (empty? subs)
      (const [])
      (subscribable aa-unsub aa-sub subs))))

(letfn [(cc-sub [target make-id-message make-value-message cnt sub]
          (actions/guard
           (fn []
             (let [values (atom (vec (repeat cnt nil)))]
               (subscribe-action sub target
                                 make-id-message
                                 (fn [[idx value]]
                                   (swap! values assoc idx value)
                                   (make-value-message @values)))))))
        (cc-unsub [id sub]
          (unsubscribe-action sub id))]
  (defn concurrent
    "Returns a subscribable that yields a vector of the values of all
  the given subs in the same order."
    [& subs]
    (if (empty? subs)
      (const [])
      (subscribable cc-unsub cc-sub (count subs) (apply indexed subs)))))

(letfn [(sq-sub [target make-id-message make-value-message sub]
          (subscribe-action sub target
                            make-id-message
                            (fn [[idx value]]
                              (make-value-message value))))
        (sq-unsub [id sub]
          (unsubscribe-action sub id))]
  (defn sequential
    "Returns a subscribable that yields the values of the given
  subs as they come in."
    [& subs]
    (if (empty? subs)
      (const [])
      (subscribable sq-unsub sq-sub (apply indexed subs)))))


(letfn [(w-sub [target make-id-message make-value-message sub size]
          (actions/guard
           (fn []
             (let [prev (atom (repeat size nil))]
               (subscribe-action sub target make-id-message
                                 (fn [value] ;; TODO: bind fn
                                   (let [res (conj (vec (rest @prev)) value)]
                                     (reset! prev res)
                                     (make-value-message res))))))))
        (w-unsub [id sub size]
          (unsubscribe-action sub id))]
  (defn sliding-window
    "Returns a subscribable on a vector of the most recent `size`
  values of `sub`. The newest value will be last. Initially the vector
  is padded with nils."
    [sub size]
    (subscribable w-unsub w-sub sub size)))
