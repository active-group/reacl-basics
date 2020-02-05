(ns reacl-basics.pages.history
  (:require [accountant.core :as accountant]
            [cemerick.url :as url]))

(defprotocol History
  (start! [this nav-path! path-exists?] "Start listening to history events, calling `(nav-path! path)` when the top of the stack changes and `(path-exists? path)` returns truthy.")
  (get-current [this] "Return the path currently on the top of the history stack.")
  (push! [this path] "Push a new path on the history stack, NOT triggering the `nav-path!` callback.")
  (stop! [this] "Stop listening to history events."))

(defrecord ^:private Html5History [auto-nav?]
  History
  (start! [_ nav-path! path-exists?]
    (accountant/configure-navigation!
     {:nav-handler (fn [path] (when-not @auto-nav? (nav-path! path)))
      :path-exists? path-exists?}))
  (get-current [_]
    ;; akin to accountang/dispatch-current!
    (let [path (-> js/window .-location .-pathname)
          query (-> js/window .-location .-search)
          hash (-> js/window .-location .-hash)]
      (str path query hash)))
  (push! [_ path]
    ;; navigate! triggers the nav-handler (even synchronous), to prevent that, we could unlisten, call it, then listen again.
    ;; but it should also ok to use a global atom for now; there can't be multiple history listers anyway.
    (try (reset! auto-nav? true)
         (accountant/navigate! path)
         (finally (reset! auto-nav? false))))
  (stop! [_]
    (accountant/unconfigure-navigation!)))

(let [glob-h (Html5History. (atom false))]
  (defn html5-history
    "Returns an implementation of the [[History]] protocol using the Html5 History API, resp. `venantius/accountant`."
    [] ;; TODO :reload-same-path? option?
    ;; Note: put a data-trigger attribute on <a> that are not client-links
    ;; Note: return value should compare = for equal arguments.
    glob-h))
