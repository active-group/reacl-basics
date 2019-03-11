(ns reacl-basics.pages.core
  "A framework for page-based application.

For a standard HTML5 history page-based application, create 3 files:

my_routes.cljc
```
(ns my-routes (:require [reacl-basics.pages.routes :as r]))

(r/defroute article \"/article/:id\")
```

my_pages.cljs
```
(ns my-pages (:require [reacl-basics.pages.core :as p]
                       [my-routes :as my]))

(defclass article-page this app-state [id]
  render ...)

(defclass main this app-state []
  render
  (html5-history-router (reacl/opt ...) app-state
    {my/article (p/page article-page)}))
```

my_client.clj
```
(ns my-client (:require [reacl-basics.pages.ring :as r]
                        [reacl-basics.pages.routes :as routes]
                        my-routes))

(-> app
    (r/wrap-client-routes routes/routes (ring-http/ok \"<html>...\")))

```


"
  (:require [reacl2.core :as reacl :include-macros true]
            [reacl-basics.core :as core :include-macros true]
            [reacl-basics.pages.router :as router]
            [reacl-basics.pages.history :as history]))

(defn page
  "Any class or function taking an `reacl/opt` value, an app-state and
  any path arguments of the route it's assigned to, plus an optional
  map of query parameters, can be used as a page. If more arguments
  are required, then use this function to bind more arguments for the
  page class. The class/function will be instantiated with the given
  `args` before the path and query paramters."
  [f & args] (if (empty? args)
               f
               (router/BoundPage. f args)))

(core/defc ^{:doc "A Reacl class that listens to navigation events and
  handles [[goto]] actions, and which renders as the
  corresponding [[page]] classes from the given map of routes to
  pages."}  html5-history-router opt app-state [pages]
  (router/history-router opt app-state
                         (history/html5-history)
                         pages))

(def ^{:doc "Returns an action to be handled by a wrapped history-router, instructing it to navigate to the given `path`, which may include query params."
       :arglists '([path])}
  goto router/goto)
