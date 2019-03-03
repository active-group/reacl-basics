(ns reacl-basics.subs.time
  "Subscribables that have to do with time."
  (:require [reacl-basics.subs.core :as core]
            [reacl-basics.actions.core :as actions]
            [reacl-basics.actions.browser :as browser])
  (:refer-clojure :exclude [delay sequence]))


(letfn [(delay-sub [target make-id-message make-value-message value ms]
          (browser/timeout target make-id-message ms (make-value-message value)))]
  (defn delay
    "Returns a subscribable that will yield `value` after a timeout of
  `ms` milliseconds, 0 by default. Note that browsers might use a
  higher minimum."
    ([value] (delay value 0))
    ([value ms]
     (core/simple browser/clear-timeout
                  delay-sub value ms))))

(letfn [(step [[make-value-message value-seq]]
          [(make-value-message (first value-seq)) [make-value-message (rest value-seq)]])
        (seq-sub [target make-id-message make-value-message value-seq ms]
          (actions/comp (actions/async-messages target (constantly (make-value-message (first value-seq))))
                        (browser/interval* target make-id-message ms step [make-value-message (rest value-seq)])))
        (seq-unsub [[id value-seq]]
          (browser/clear-timeout id))]
  (defn sequence
    "Returns a subscribable that will yield all values from the
  given (lazy) sequence in `ms` millisecond stepts. The first value
  will be available immediately."
    [value-seq ms]
    (core/simple browser/clear-timeout
                 seq-sub value-seq ms)))

;; current time? (as a map over infinite sequence?)

(def animation-frames
  "A subscribable on the animation frames of the browser,
  which will yield new timestamp values (a DOMHighResTimeStamp, which
  is a double) that can be used to update an animation
  accordingly. The rate is typically 60 times per second but can
  vary."
  (core/simple browser/cancel-animation-frames
               browser/request-animation-frames))
