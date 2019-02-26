(ns reacl-basics.actions.browser
  (:require [reacl-basics.actions.core :as core]
            goog.async.nextTick))

;; TODO: is 'browser' the right package? Esp. for the timers?

(defn clear-timeout
  "Returns an action that will clear the timeout with the given id."
  [id]
  (core/external-action js/window.clearTimeout id))

(letfn [(tmo [send! make-id-message ms message]
          (make-id-message (js/window.setTimeout (fn []
                                                   (send! message)) ms)))]
  (defn timeout
    "Returns an action that will send `message` to `target` after `ms`
  milliseconds. It will also send the message `(make-id-message id)`
  to it with an id that can be used to clear the timeout
  with [[clear-timeout]]."
    [target make-id-message ms message]
    (core/message-action target tmo make-id-message ms message)))

(letfn [(ntk [send! message]
          (goog.async.nextTick (fn []
                                 (send! message))))]
  (defn next-tick
    "Returns an action that will send `message` to `target`
  immediately after the control goes back to the browser. Similar to
  a [[timeout]] with 0 milliseconds."
    [target message]
    ;; Note: next-tick cannot be cancelled. (don't know if that is a flaw in goog.async.nextTick or not)
    (core/message-action target ntk message)))

(defn restart-timeout
  "Returns an action that tries to clear the timeout with the given
  `prev-id`, if not `nil`, and then starts a new [[timeout]] with the given args."
  [prev-id make-id-message ms message]
  (cond->> (timeout make-id-message ms message)
    prev-id (core/comp-actions (clear-timeout prev-id))))

(defn clear-interval
  "Returns an action that will clear the interval with the given id."
  [id]
  (core/external-action js/window.clearTimeout id))

(letfn [(intv [send! make-id-message ms message-fn initial-state]
          (let [state (atom initial-state)]
            (make-id-message (js/window.setInterval (fn []
                                                      (let [[msg nstate] (message-fn state)]
                                                        (reset! state nstate)
                                                        (send! msg)))
                                                    ms))))]
  (defn interval*
    "Returns an action that will call `(message-fn state)` which must
  yield a pair of a message and a new state, which is sent to
  `target`. Initially that will be repeated every `ms`
  milliseconds. It will also send the message `(make-id-message id)`
  to it with an id that can be used to clear the timeout
  with [[clear-interval]]."
    ([target make-id-message ms message-fn]
     (interval* target make-id-message ms message-fn nil))
    ([target make-id-message ms message-fn initial-state]
     (core/message-action target intv make-id-message ms message-fn initial-state))))

(defn interval
  "Returns an action that will send `message` to `target` every `ms`
  milliseconds. It will also send the message `(make-id-message id)`
  to it with an id that can be used to clear the timeout
  with [[clear-interval]]."
  [target make-id-message ms message]
  (interval* target make-id-message ms vector message))

(defn go-back
  "Returns an action that will instruct the browser to go one step back in its history."
  []
  (core/external-action js/window.history.back))

;; TODO: window.open, window.print ?
(defn reload
  "Returns an action that will instruct the browser to reload the
  page, optionally by clearing its caches before that."
  ([] (reload false))
  ([clear-cache?]
   (core/external-action js/window.location.reload (boolean clear-cache?))))

(defn cancel-animation-frame [id]
  (core/external-action js/window.cancelAnimationFrame id))

(letfn [(raf [send! make-id-message make-message]
          (make-id-message (js/window.requestAnimationFrame (fn [timestamp]
                                                              (send! (make-message timestamp))))))]
  (defn request-animation-frame
    "Returns an action that will send `(make-message timestamp)` to
  `target` on the next animation frame, and `(make-id-message id)`
  with an id that can be used with `cancel-animation-frame`
  immediately."
    [target make-id-message make-message]
    (core/message-action target raf make-id-message make-message)))

(defn cancel-animation-frames [id]
  (core/external-action js/window.cancelAnimationFrame @id))

(letfn [(rafs [send! make-id-message make-message]
          (let [id (atom nil)
                action (fn action [timestamp]
                         (send! (make-message timestamp))
                         (reset! id (js/window.requestAnimationFrame action)))]
            (reset! id (js/window.requestAnimationFrame action))
            (make-id-message id)))]
  (defn request-animation-frames
    "Returns an action that will repeatedly send `(make-message timestamp)` to
  `target` on every next animation frame, and `(make-id-message id)`
  with an id that can be used with `cancel-animation-frames`
  immediately."
    [target make-id-message make-message]
    (core/message-action target rafs make-id-message make-message)))

;; scrolling
;; resizing

;; matchMedia?
