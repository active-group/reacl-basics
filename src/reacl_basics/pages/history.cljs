(ns reacl-basics.pages.history
  (:require [accountant.core :as accountant]
            [cemerick.url :as url]))

(defprotocol History
  (start! [this nav-path! path-exists?] "Start listening to history events, calling `(nav-path! path) when the top of the stack changes and `(path-exists? path)` returns truthy.")
  (get-current [this] "Return the path currently on the top of the history stack.")
  (push! [this path] "Push a new path on the history stack, NOT triggering the `nav-path!` callback.")
  (stop! [this] "Stop listening to history events."))

(defn html5-history [] ;; TODO :reload-same-path? option?
  ;; Note: put a data-trigger attribute on <a> that are not client-links
  (reify History
    (start! [_ nav-path! path-exists?]
      (accountant/configure-navigation!
       {:nav-handler nav-path! :path-exists? path-exists?}))
    (get-current [_]
      ;; akin to accountang/dispatch-current!
      (let [path (-> js/window .-location .-pathname)
            query (-> js/window .-location .-search)
            hash (-> js/window .-location .-hash)]
        (str path query hash)))
    (push! [_ path]
      ;; FIXME if synchronous, then disable listener; push; enable again; then return a :message?
      (accountant/navigate! path))
    (stop! [_]
      ;; FIXME: commited, but not release by accountant yet: (accountant/unconfigure-navigation!)
      nil)))
