(ns reacl-basics.actions.browser
  (:require [reacl-basics.actions.core :as core]
            goog.async.nextTick))

(defn clear-timeout [id]
  (core/external-action js/window.clearTimeout js/window id))

(letfn [(tmo [send! make-id-message ms message]
          (make-id-message (js/window.setTimeout (fn []
                                                   (send! message)) ms)))]
  (defn timeout
    [target make-id-message ms message]
    (core/message-action target tmo make-id-message ms message)))

(letfn [(ntk [send! message]
          (goog.async.nextTick (fn []
                                 (send! message))))]
  (defn next-tick [target message]
    ;; Note: next-tick cannot be cancelled. (don't know if that is a flaw in goog.async.nextTick or not)
    (core/message-action target ntk message)))

(defn restart-timeout
  "Returns an action that tries to cancel the timeout with the given
  prev-id, and then starts a new timeout with the given args."
  [prev-id make-id-message ms message]
  (cond->> (timeout make-id-message ms message)
    prev-id (core/comp-actions (clear-timeout prev-id))))

(defn go-back []
  (core/external-action js/window.history.back))

(defn reload
  ([] (reload false))
  ([clear-cache?]
   (core/external-action js/window.location.reload (boolean clear-cache?))))
