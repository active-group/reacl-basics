(ns reacl-basics.pages.routes
  (:require [clout.core :as clout]
            [clojure.string :as str]
            [cemerick.url :as url]))

;; TODO: prefix configuration?

(def routes (atom #{})) ;; Note: only used for the ring middleware.

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
     "If the given URI matches the given route, returns a vector of all patch arguments and optionally a map of query params."
     [route uri]
     (-parse-uri route uri)))

#?(:clj
   (defprotocol ^:private IRoutable
     (-route-matches [this request])))

#?(:clj
   (defn route-matches [route request]
     (-route-matches route request)))

(defn routable? [v]
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

#?(:cljs
   (defn ^:no-doc route [pattern]
     (Route. (clout/route-compile pattern))))

(defn clear-routes! []
  (reset! routes #{}))

(defn- clojure? [env]
  (not (contains? env :js-globals)))

(defmacro defroute [name pattern]
  (let [positional (:keys (clout/route-compile pattern))
        pargs (mapv (fn [_] (gensym "arg")) positional)
        oargs (mapv (comp symbol clojure.core/name) positional)]
    `(def ~name
       (let [r# (route ~pattern)]
         (swap! routes conj r#)
         r#))))
