(ns reacl-basics.pages.core
  "A framework for page-based application.

  TODO explain
"
  (:require [reacl-basics.core :as core :include-macros true]
            [reacl2.core :as reacl :include-macros true]
            [reacl-basics.pages.routes :as routes]
            [reacl-basics.pages.router :as router]))

(defprotocol ^:no-doc IPage
  (page-id [this])
  (render-page! [this opt app-state args]))

(defrecord ^:private Page [id f f-args]
  IPage
  (page-id [this] id)
  (render-page! [_ opt app-state p-args]
    (apply f opt app-state
           (concat f-args p-args))))

(defn page [id f & args]
  (Page. id f args))

(defn routable-page? [page]
  (routes/routable? (page-id page)))

(defrecord ^:private AppState [value])

(defrecord ^:private GotoPage [id args])
(defrecord ^:private SetPage [id args])

(defn goto-page [id & args]
  (GotoPage. id args))

(defn- find-page [pages id]
  (first (filter #(= id (page-id %)) pages)))

;; current = [id args] or href

;; TODO: option to use app-state to store path (if no router is used?)
;; TODO: initial dispatch (after page load)
(reacl/defclass pager this app-state [options pages]
  local-state [state {:current {:page (first pages) :args nil}
                      :red-act (fn [_ action]
                                 (condp instance? action
                                   GotoPage (reacl/return :message [this action])
                                   (reacl/return :action action)))}]

  component-will-mount
  (fn []
    (if (:use-router? options)
      (reacl/return :action (router/register-pager this (map page-id (filter routable-page? pages))
                                                   ->SetPage))
      (reacl/return)))

  component-will-unmount
  (fn []
    (if (:use-router? options)
      (reacl/return :action (router/unregister-pager this))
      (reacl/return)))
  
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

      SetPage (reacl/return :local-state (let [page (find-page pages (:id msg))] ;; TODO: what if not found?
                                           (assoc state
                                                  :current {:page page
                                                            :args (:args msg)})))
      GotoPage (let [id (:id msg)]
                 ;; To go to a page, we either push it to the history and wait for its callback (SetPage),
                 ;; or change our state directly.
                 (if-let [page (find-page pages id)]
                   (if (and (:use-router? options) (routable-page? page))
                     (reacl/return :action (router/push (routes/href page (:args msg))))
                     (reacl/return :local-state (assoc state :current {:page page :args (:args msg)})))
                   ;; TODO: other kind of error?
                   (do (js/console.warn "Page not found:" id)
                       (reacl/return)))))))

(defn- lens-id
  ([v] v)
  ([_ v] v))

(reacl/defclass router-with-pager this app-state [options history pages]
  render
  (router/router history
                 (pager (reacl/opt :embed-app-state lens-id) app-state
                        (-> options
                            (assoc :use-router? true))
                        pages)))
