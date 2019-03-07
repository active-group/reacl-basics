(ns reacl-basics.actions.ajax
  "Action for making Ajax requests, based on clj-ajax."
  (:require [reacl-basics.actions.core :as core]
            [ajax.core :as ajax]))

(defn ^:no-doc check-ajax-action-options-invariants! [options]
  (assert (not (:handler options)))
  (assert (not (:error-handler options))))

(letfn [(aj-act [send! make-id-message make-value-message f uri options]
          (let [nopts (assoc options
                             :handler
                             (fn [response]
                               (send! (make-value-message [true response])))
                             :error-handler
                             (fn [error]
                               ;; the 'aborted' error is sent when we cancel the request, but a subscription must not send anything after that.
                               (when (not= :aborted (:failure error))
                                 (send! (make-value-message [false error])))))]
            (make-id-message (f uri nopts))))]
  (defn- ajax-action [target make-id-message make-value-message f uri options]
    (check-ajax-action-options-invariants! options)
    (core/async-messages target
                         aj-act
                         make-id-message make-value-message f uri options)))

(defn GET [target make-id-message make-value-message uri options] (ajax-action target make-id-message make-value-message ajax/GET uri options))
(defn HEAD [target make-id-message make-value-message uri options] (ajax-action target make-id-message make-value-message ajax/HEAD uri options))
(defn POST [target make-id-message make-value-message uri options] (ajax-action target make-id-message make-value-message ajax/POST uri options))
(defn PUT [target make-id-message make-value-message uri options] (ajax-action target make-id-message make-value-message ajax/PUT uri options))
(defn DELETE [target make-id-message make-value-message uri options] (ajax-action target make-id-message make-value-message ajax/DELETE uri options))
(defn OPTIONS [target make-id-message make-value-message uri options] (ajax-action target make-id-message make-value-message ajax/OPTIONS uri options))
(defn TRACE [target make-id-message make-value-message uri options] (ajax-action target make-id-message make-value-message ajax/TRACE uri options))
(defn PATCH [target make-id-message make-value-message uri options] (ajax-action target make-id-message make-value-message ajax/PATCH uri options))
(defn PURGE [target make-id-message make-value-message uri options] (ajax-action target make-id-message make-value-message ajax/PURGE uri options))

(defn ^:no-doc check-ajax-request-action-options-invariants! [options]
  (assert (not (:handler options))))

(letfn [(ajr-act [send! make-id-message make-value-message options]
          (let [nopts (assoc options
                             :handler
                             (fn [response]
                               ;; the 'aborted' error is sent when we cancel the request, but a subscription must not send anything after that.
                               (when (or (first response)
                                         (not= :aborted (:failure (second response))))
                                 (send! (make-value-message response)))))]
            (make-id-message (ajax/ajax-request nopts))))]
  (defn ajax-request [target make-id-message make-value-message options]
    (assert (not (:handler options)))
    (core/async-messages target ajr-act make-id-message make-value-message options)))

(defn ajax-abort [id]
  (core/external ajax/abort id))
