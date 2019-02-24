(ns reacl-basics.actions.browser
  (:require [reacl-basics.actions.core :as core]
            goog.async.nextTick))

;; TODO: is 'browser' the right package? Esp. for the timers?

(defn clear-timeout
  "Returns an action that will clear the browser timeout with the given id."
  [id]
  (core/external-action js/window.clearTimeout js/window id))

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

(defn go-back
  "Returns an action that will instruct the browser to go one step back in its history."
  []
  (core/external-action js/window.history.back))

(defn reload
  "Returns an action that will instruct the browser to reload the
  page, optionally by clearing its caches before that."
  ([] (reload false))
  ([clear-cache?]
   (core/external-action js/window.location.reload (boolean clear-cache?))))
