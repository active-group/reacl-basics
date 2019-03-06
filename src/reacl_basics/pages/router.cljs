(ns reacl-basics.pages.router
  (:require [reacl2.core :as reacl]
            [reacl2.core :as reacl :include-macros true]
            [reacl-basics.core :as core]
            [reacl-basics.pages.history :as history]
            [reacl-basics.pages.routes :as routes]))

(defrecord ^:private PushUri [uri])

(defn push [uri]
  (PushUri. uri))

(defrecord ^:private RegisterPager [pager pages goto-msg])

(defn register-pager [pager pages goto-msg]
  (RegisterPager. pager pages goto-msg))

(defrecord ^:private GotoUri [uri])

(reacl/defclass router this [history content]
  local-state [state
               {:pagers {} ;; pager -> [pages goto-msg] (usually only one)

                :listener (fn [uri]
                            (reacl/send-message! this (GotoUri. uri)))
                :act-red (fn [_ action]
                           (condp instance? action
                             PushUri (do
                                       (history/push! history (:uri action))
                                       (reacl/return))
                             RegisterPager (reacl/return :message [this action])
                             (reacl/return :action action)))}]

  component-will-mount
  (fn []
    (history/listen! history (:listener state))
    (reacl/return))

  component-will-unmount
  (fn []
    (history/unlisten! history (:listener state))
    (reacl/return))

  render
  (core/reduce-action content
                      (:act-red state))

  handle-message
  (fn [msg]
    (condp instance? msg
      GotoUri (let [uri (:uri msg)
                    [pager goto-msg page page-args]
                    (reduce-kv (fn [res pager [pages goto-msg]]
                                 (or res
                                     (if-let [page (first (filter #(not= false (routes/parse % uri))
                                                                  pages))]
                                       [pager goto-msg page (routes/parse page uri)]
                                       nil)))
                               nil
                               (:pagers state))]
                (if page
                  (reacl/return :message [pager (goto-msg page page-args)])
                  ;; TODO: error?
                  (reacl/return)))
      
      RegisterPager (reacl/return :local-state (assoc state :pagers
                                                      (:pager msg) [(:pages msg) (:goto-msg msg)])))))
