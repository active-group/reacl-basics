(ns reacl-basics.subscriptions.time
  "Subscriptions that have to do with time."
  (:require [reacl-basics.subscriptions.core :as core]
            [reacl-basics.actions.browser :as browser]))

(letfn [(delay-sub [target make-id-message make-value-message value ms]
          (browser/timeout target make-id-message ms (make-value-message value)))]
  (defn delay
    "Returns a subscription that will yield `value` after a timeout of `ms` milliseconds."
    [value ms]
    (core/simple-subscription browser/cancel-timeout
                              delay-sub value ms)))

#_(defn with-delay [value ms f & args]
  (apply with-subscription (delay value ms) f args))
