(ns reacl-basics.pages.router
  (:require [reacl2.core :as reacl]
            [reacl2.core :as reacl :include-macros true]
            [reacl-basics.core :as core]
            [reacl-basics.pages.history :as history]
            [reacl-basics.pages.routes :as routes]))

(defrecord ^:private PushUri [uri])

(defn push [uri]
  (PushUri. uri))

(defrecord ^:private RegisterPager [pager routes goto-msg])

(defrecord ^:private UnregisterPager [pager])

(defn register-pager [pager routes goto-msg]
  (RegisterPager. pager routes goto-msg))

(defn unregister-pager [pager]
  (UnregisterPager. pager))

(defrecord ^:private GotoUri [uri])

(defn- history-start! [history state]
  (let [all-routes (apply concat (map (comp first second) (:pagers state)))]
    (history/start! history (:listener state)
                    (fn [path]
                      (some #(routes/parse % path) all-routes)))))

(reacl/defclass router this [history content]
  local-state [state
               {:pagers {} ;; pager -> [routes goto-msg] (usually only one)

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
    (history-start! history state)
    (reacl/return))

  component-did-update
  (fn []
    ;; esp. when the pagers change initially.
    (history/stop! history)
    (history-start! history state)
    (reacl/return))

  component-will-unmount
  (fn []
    (history/stop! history)
    (reacl/return))

  render
  (core/reduce-action content
                      (:act-red state))

  handle-message
  (fn [msg]
    (condp instance? msg
      GotoUri
      (let [uri (:uri msg)
            [pager goto-msg page page-args]
            (reduce-kv (fn [res pager [routes goto-msg]]
                         (or res
                             (if-let [page (first (filter #(routes/parse % uri)
                                                          routes))]
                               [pager goto-msg page (routes/parse page uri)]
                               nil)))
                       nil
                       (:pagers state))]
        (if page
          (reacl/return :message [pager (goto-msg page page-args)])
          ;; TODO: other kind of error? not-found fallback?
          (do (js/console.warn "No page found for route:" uri)
              (reacl/return))))
      
      UnregisterPager
      (reacl/return :local-state (update-in state [:pagers] dissoc (:pager msg)))
      
      RegisterPager
      (reacl/return :local-state (assoc-in state [:pagers (:pager msg)]
                                           [(:routes msg) (:goto-msg msg)])))))
