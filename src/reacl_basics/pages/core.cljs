(ns reacl-basics.pages.core
  "A framework for page-based application.

  TODO explain
"
  (:require [reacl-basics.core :as core :include-macros true]
            [reacl2.core :as reacl :include-macros true]
            [reacl-basics.pages.routes :as routes]
            [reacl-basics.pages.router :as router]))

(defprotocol IPage
  (page-id [this])
  (render-page! [this opt app-state args]))

(defrecord Page [id f f-args]
  IPage
  (page-id [this] id)
  (render-page! [_ opt app-state p-args]
    (apply f opt app-state
           (concat f-args p-args))))

(defn page [id f & args]
  (Page. id f args))

(defn routable-page? [page]
  (routes/routable? (:id page)))

(defrecord ^:private AppState [value])

(defrecord ^:private GotoPage [id args])
(defrecord ^:private SetPage [page args])

(defn goto-page [id & args]
  (GotoPage. id args))

(reacl/defclass pager this app-state [pages & [use-router?]]
  local-state [state {:current {:page (first pages) :args nil}
                      :red-act (fn [_ action]
                                 (condp instance? action
                                   GotoPage (reacl/return :message [this action])
                                   (reacl/return :action action)))}]

  component-will-mount
  (fn []
    (if use-router?
      (reacl/return :action (router/register-pager this (filter routable-page? pages)
                                                   ->SetPage))
      (reacl/return)))

  component-will-unmount
  (fn []
    ;; TODO: unregister, update-pager.
    (reacl/return))
  
  render
  (let [current (:current state)
        red-act (:red-act state)]
    (render-page! (:page current) (reacl/opt :reaction (reacl/reaction this ->AppState)
                                             :reduce-action red-act)
                  app-state
                  (:args current)))

  handle-message
  (fn [msg]
    (condp instance? msg
      AppState (reacl/return :app-state (:value msg))

      SetPage (reacl/return :local-state (assoc state
                                                :current {:page (:page msg)
                                                          :args (:args msg)}))
      GotoPage (let [id (:id msg)]
                 (if-let [page (first (filter #(= id (page-id %)) pages))]
                   (if (and use-router? (routable-page? page))
                     (reacl/return :action (router/push (routes/href page (:args msg))))
                     (reacl/return :local-state (assoc state :current {:page page :args (:args msg)})))
                   ;; TODO: error?
                   (reacl/return))))))

(core/defc router-with-pager opt app-state [history pages]
  (router/router history
                 (pager opt app-state pages true)))
