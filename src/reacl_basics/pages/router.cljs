(ns reacl-basics.pages.router
  (:require [reacl2.core :as reacl :include-macros true]
            [reacl-basics.core :as core :include-macros true]
            [active.clojure.lens :as lens]
            [reacl-basics.pages.history :as history]
            [reacl-basics.pages.routes :as routes]))

(defrecord ^:private BoundPage [f args])

(defn- render-page [opt state page & path-args]
  (if (instance? BoundPage page)
    (apply (:f page) opt state (concat (:args page) path-args))
    (apply page opt state path-args)))

(defrecord ^:private Show [id args]) ;; 'parsed'

(defn show
  "Returns a message to be to be sent to a [[router]], instructing it
  to show the page registered for the given `id` and additional
  `args`."
  [id & args]
  (Show. id args))

(core/defc dispatch-view opt app-state [pages id & args]
  (apply render-page
         opt
         app-state
         (let [page (get pages id nil)]
           ;; FIXME: take a 'page not found' placeholder? throw?
           (when-not page
             (js/console.warn "Page not found:" id "in" (keys pages)))
           [page args])))

(reacl/defclass ^{:arglists '([router-state-lens page-state-lens pages])
                  :doc "A class that dispatches rendering based on the
  given map of id to pages, where a value `[id args]` should be in the
  app-state under `router-state-lens`, and the pages are instantited
  over the same part of the app-state under `page-state-lens`. First
  argument to the page is always this router."}  router this
  app-state [router-state-lens page-state-lens pages]
  
  render
  (apply dispatch-view
         (reacl/opt :embed-app-state page-state-lens)
         (page-state-lens app-state)
         pages
         (let [[id args] (router-state-lens app-state)]
           (cons id (cons this args))))

  handle-message
  (fn [msg]
    (condp instance? msg
      Show (reacl/return :app-state (router-state-lens app-state [(:id msg) (:args msg)])))))

(defn- get-parsed [pages uri] ;; => [key args]
  (or (some (fn [[route page]]
              (when-let [args (routes/parse route uri)]
                [route args]))
            pages)
      (js/console.warn "Route not found:" uri)
      nil))

(defn- page-exists-fn? [pages]
  (fn [uri]
    (some? (get-parsed pages uri))))

(defrecord ^:private Goto [uri])

(defn goto
  "Returns an action to be handled by a [[history-router]] up in the
  hierarchy, instructing it to show the page registered for the given
  `path` (and query string)."
  [path]
  (Goto. path))

(reacl/defclass ^{:doc "A class that dispatches rendering based on the
  given map of routes to pages, where the current route and route
  changes are managed by the given implementation of
  the [[reacl-basics.pages.history/History]] protocol."
                  :arglists '([history pages])}
  history-router
  this app-state [history pages]

  local-state [state {:red-act (fn [_ action history]
                                 (condp instance? action
                                   Goto (do (history/push! history (:uri action))
                                            (reacl/return :message [this action]))
                                   (reacl/return :action action)))
                      :current (get-parsed pages (history/get-current history))
                      :listener (fn [uri]
                                  (reacl/send-message! this (Goto. uri)))}]
  component-will-mount
  (fn []
    (history/start! history
                    (:listener state)
                    (page-exists-fn? pages))
    (reacl/return))

  component-did-update
  (fn [_ _ new-history new-pages]
    (when (not= [new-history new-pages]
                [history pages])
      (history/stop! history)
      (history/start! new-history
                      (:listener state)
                      (page-exists-fn? new-pages)))
    (reacl/return))

  render
  (apply dispatch-view
         (reacl/opt :embed-app-state lens/id
                    :reduce-action (core/action-reducer (:red-act state) history))
         app-state
         pages
         (:current state))
  
  component-will-unmount
  (fn []
    (history/stop! history))

  handle-message
  (fn [msg]
    (condp instance? msg
      Goto (reacl/return :local-state (assoc state :current
                                             (get-parsed pages (:uri msg)))))))
