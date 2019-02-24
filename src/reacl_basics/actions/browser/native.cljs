(ns reacl-basics.actions.browser.native
  (:require goog.async.nextTick))

(defn set-timeout! [ms callback]
  (js/window.setTimeout callback ms))

(defn clear-timeout! [id]
  (js/window.clearTimeout js/window id))

(defn next-tick! [thunk]
  (goog.async.nextTick thunk))

(defn go-back! []
  (js/window.history.back))

(defn reload! [& [clear-cache?]]
  (js/window.location.reload (boolean clear-cache?)))
