(ns reacl-basics.pages.history
  (:require [accountant.core :as accountant]
            [cemerick.url :as url]))

(defprotocol History
  (push! [this path])
  (start! [this nav-path! path-exists?])
  (stop! [this]))

(defn html5-history [] ;; :reload-same-path?
  ;; need (dispatch-current!) to start?
  ;; put data-trigger on <a> that are not client-links
  (reify History
    (push! [_ path]
      ;; Note: if all history impls. take path and query separately, when change push! ? (we just created the query-string; not parsing it again)
      (let [u (url/url path)]
        (accountant/navigate! (:path u) (:query u))))
    (stop! [_]
      ;; FIXME: commited, but not release by accountant yet: (accountant/unconfigure-navigation!)
      nil)
    (start! [_ nav-path! path-exists?]
      (accountant/configure-navigation!
       {:nav-handler nav-path! :path-exists? path-exists?}))))
