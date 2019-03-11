(ns reacl-basics.pages.routes
  (:require [clout.core :as clout]
            [clojure.string :as str]
            [cemerick.url :as url]))

;; TODO: prefix configuration?

(def ^{:doc "An atom with a set of all routes created by [[defroute]] since the last call to [[clear-routes!]]."}
  defined-routes (atom #{})) ;; Note: only used for the ring middleware.

#?(:cljs
   (defprotocol ^:private IRoutable
     (-unparse-uri [this params] "Return uri for this routable, using params as path or query params.")
     (-parse-uri [this uri] "Return vector of positional and a map of query params if this uri matches")))

#?(:cljs
   (defn href
     "Returns a relative URI for the given route and concrete values for all parameters. The last parameter may be a map of query params."
     [route & params]
     (-unparse-uri route params)))

#?(:cljs
   (defn parse
     "If the given URI matches the given route, returns a vector of all path arguments and optionally a map of query params."
     [route uri]
     (-parse-uri route uri)))

#?(:clj
   (defprotocol ^:private IRoutable
     (-route-matches [this request])))

#?(:clj
   (defn route-matches [route request]
     (-route-matches route request)))

(defn routable?
  "If `v` is a value returned by [[route]] of bound to a name with [[defroute]]."
  [v]
  (satisfies? IRoutable v))

(defn- parse-request [c-pattern request]
  (let [b (clout/route-matches c-pattern request)]
    (when b
      (vec (concat (map b (:keys c-pattern))
                   (some-> (not-empty (into {} (map (fn [[k v]] [(keyword k) v]) (:query-params request))))
                           (vector)))))))

#?(:cljs
   (defrecord ^:private Route [c-pattern]
              IRoutable
              (-unparse-uri [_ args]
                (let [{:keys [source re keys absolute?]} c-pattern
                      _ (assert (or (= (count args) (count keys))
                                    (= (count args) (inc (count keys)))))
                      positional (map vector
                                      keys
                                      (take (count keys) args))
                      params (if (= (count args) (count keys))
                               nil
                               (last args))]
                  (let [s (reduce (fn [s [k v]]
                                    (str/replace s (str k) (url/url-encode (str v))))
                                  source
                                  positional)]
                    (if (empty? params)
                      s
                      (str s "?" (str/join "&" (map (fn [[k v]]
                                                      (str (url/url-encode (name k)) "=" (url/url-encode (str v))))
                                                    params)))))))
              (-parse-uri [this uri]
                (let [furl (url/url uri)]
                  ;; in particular, we must remove the query params to use clout:
                  (parse-request c-pattern {:uri (:path furl)
                                            :query-params (:query furl)})))))

#?(:clj
   (defrecord ^:private Route [c-pattern]
     IRoutable 
     (-route-matches [_ request] (parse-request c-pattern request))))

(defn ^:no-doc route [pattern]
  (Route. (clout/route-compile pattern)))

(defn clear-routes!
  "Clears the global set of [[defined-routes]]."
  []
  (reset! defined-routes #{}))

(defn- clojure? [env]
  (not (contains? env :js-globals)))

(defmacro defroute
  "Defines a var with the given `name` to be a route according the given `pattern`, and adds it to the global set of [[defined-routes]].

A pattern can be a fixed path `\"/path/to/page\"`, or can have one or more path arguments `\"/article/:id\"`.

For example:

```clojure
(defroute r0 \"/home\")

(href r0) => \"/home\"
(parse r0 \"/home\") => []
(href r0 {:lang \"de\"}) => \"/home?lang=de\"
(parse r0 \"/home?lang=de\") => [{:lang \"de\"}]

(defroute r1 \"/path/:id\")

(href r1 \"123\") => \"/path/123\"
(parse r1 \"/path/123\") => [\"123\"]
(parse r1 \"/other/path\") => nil
```
"
  [name pattern]
  (let [positional (:keys (clout/route-compile pattern))
        pargs (mapv (fn [_] (gensym "arg")) positional)
        oargs (mapv (comp symbol clojure.core/name) positional)]
    `(def ~(vary-meta name assoc
                      :doc (str "Route with the pattern " (pr-str pattern) ".")
                      :arglists (list oargs (conj oargs `'query-params)))
       (let [r# (route ~pattern)]
         (swap! defined-routes conj r#)
         r#))))
