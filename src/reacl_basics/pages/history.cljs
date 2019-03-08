(ns reacl-basics.pages.history
  (:require [accountant.core :as accountant]
            [cemerick.url :as url]))

(defprotocol History
  (start! [this nav-path! path-exists?] "Start listening to history events, calling `(nav-path! path) when the top of the stack changes and `(path-exists? path)` returns truthy.") ;; FIXME: what about query? and hash?
  (dispatch-current! [this] "Trigger the history event for the current top of the history event.")
  (push! [this path] "Push a new path (+ query) on the history stack, eventually triggering the `nav-path!` callback.") ;; TODO: really trigger listener?
  (stop! [this] "Stop listening to history events."))

(defn html5-history [] ;; :reload-same-path?
  ;; need (dispatch-current!) to start?
  ;; put data-trigger on <a> that are not client-links
  (reify History
    (start! [_ nav-path! path-exists?]
      (accountant/configure-navigation!
       {:nav-handler nav-path! :path-exists? path-exists?}))
    (dispatch-current! [_]
      (accountant/dispatch-current!))
    (push! [_ path]
      ;; TODO: if this is called from router/PushUri, then maybe disable listener; push; enable again; then return a :message?
      
      ;; Note: if all history impls. take path and query separately, when change push! ? (we just created the query-string; not parsing it again)
      (let [u (url/url path)]
        (accountant/navigate! (:path u) (:query u))))
    
    (stop! [_]
      ;; FIXME: commited, but not release by accountant yet: (accountant/unconfigure-navigation!)
      nil)
    ))
