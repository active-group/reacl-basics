(ns reacl-basics.pages.router
  (:require [reacl2.core :as reacl :include-macros true]
            [reacl-basics.core :as core]
            [active.clojure.lens :as lens]
            [reacl-basics.pages.history :as history]
            [reacl-basics.pages.routes :as routes]))

(defrecord ^:private BoundPage [f args])

(defn- render-page [opt state page & path-args]
  (if (instance? BoundPage page)
    (apply (:f page) opt state (concat (:args page) path-args))
    (apply page opt state path-args)))

(defrecord ^:private Show [id args]) ;; 'parsed'

(defn show [id & args]
  (Show. id args))

(reacl/defclass router this app-state [router-state-lens page-state-lens pages]
  local-state [state {:act-red (fn [_ action]
                                 (condp instance? action
                                   Show (reacl/return :message [this action])
                                   (reacl/return :action action)))}]

  render
  (core/reduce-action (apply render-page
                             (reacl/opt :reduce-action (:red-act state)
                                        :embed-app-state page-state-lens)
                             (page-state-lens app-state)
                             (let [[id args] (router-state-lens app-state)
                                   page (get pages id nil)]
                               ;; FIXME: take a 'page not found' class?
                               (when-not page
                                 (js/console.warn "Page not found:" id "in" (keys pages)))
                               [page args]))
                      (:act-red state))

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

(defn goto [uri]
  (Goto. uri))

(letfn [(c-l-yank [_ v] v)
        (c-l-shove [v _ _] v)]
  (defn lens-const [v]
    (lens/lens c-l-yank c-l-shove v)))

(reacl/defclass history-router this app-state [history pages]
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
  (router (reacl/opt :embed-app-state lens/id
                     :reduce-action (core/action-reducer (:red-act state) history))
          app-state
          ;; Note: the router is not supposed to change the page - i.e. not use 'show'.
          (lens-const (:current state))
          lens/id
          pages)

  component-will-unmount
  (fn []
    (history/stop! history))

  handle-message
  (fn [msg]
    (condp instance? msg
      Goto (reacl/return :local-state (assoc state :current
                                             (get-parsed pages (:uri msg)))))))


