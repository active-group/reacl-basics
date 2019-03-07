(ns reacl-basics.subs.server
  "Subscriptions to Ajax and Websockets."
  (:require [reacl-basics.actions.ajax :as ajax]
            [reacl-basics.actions.core :as actions]
            [reacl-basics.subs.core :as subs]))

(defn- ajax-sub [act uri options]
  (ajax/check-ajax-action-options-invariants! options)
  (subs/simple ajax/ajax-abort act uri options))

(defn GET [uri options] (ajax-sub ajax/GET uri options))
(defn HEAD [uri options] (ajax-sub ajax/HEAD uri options))
(defn POST [uri options] (ajax-sub ajax/POST uri options))
(defn PUT [uri options] (ajax-sub ajax/PUT uri options))
(defn DELETE [uri options] (ajax-sub ajax/DELETE uri options))
(defn OPTIONS [uri options] (ajax-sub ajax/OPTIONS uri options))
(defn TRACE [uri options] (ajax-sub ajax/TRACE uri options))
(defn PATCH [uri options] (ajax-sub ajax/PATCH uri options))
(defn PURGE [uri options] (ajax-sub ajax/PURGE uri options))

(defn ajax-request [options]
  (ajax/check-ajax-request-action-options-invariants! options)
  (subs/simple ajax/ajax-abort ajax/ajax-request options))

