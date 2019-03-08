(ns reacl-basics.pages.core
  "A framework for page-based application."
  (:require [reacl2.core :as reacl :include-macros true]
            [reacl-basics.core :as core :include-macros true]
            [reacl-basics.pages.router :as router]
            [reacl-basics.pages.history :as history]))

(defn page
  ([f] f)
  ([f a & args] (router/BoundPage. f (cons a args))))

(core/defc html5-history-router opt app-state [pages]
  (router/history-router opt app-state
                         (history/html5-history)
                         pages))

(def goto router/goto)
(def history-router router/history-router)

(def show router/show)
(def internal-router router/router)
