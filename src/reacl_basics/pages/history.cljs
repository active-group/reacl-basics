(ns reacl-basics.pages.history)

(defprotocol History
  (push! [this uri])
  (listen! [this f])
  (unlisten! [this f]))
