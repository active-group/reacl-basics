(ns reacl-basics.pages.routes
  (:require [clout.core :as clout]
            [clojure.string :as str]
            [cemerick.url :as url]))

;; TODO: prefix configuration?

#?(:cljs
   (defprotocol ^:private IRoutable
     (-unparse-uri [this params] "Return uri for this routable, using params as path or query params.")
     (-parse-uri [this uri] "Return map of params if this uri matches, or *false* otherwise.")))

#?(:cljs
   (defn href [route & [params]]
     (-unparse-uri route params)))

#?(:cljs
   (defn parse [route uri]
     (-parse-uri route uri)))

#?(:clj
   (defprotocol ^:private IRoutable
     (-route-matches [this request])))

#?(:clj
   (defn route-matches [route request]
     (-route-matches route request)))

(defn routable? [v]
  (satisfies? IRoutable v))

#?(:cljs
   (defrecord ^:private Route [c-pattern]
              IRoutable
              (-unparse-uri [this params]
                (let [{:keys [source re keys absolute?]} c-pattern]
                  (let [ks (set keys)
                        s (reduce (fn [s [k v]]
                                    (if (contains? ks k)
                                      (str/replace s (str k) (url/url-encode (str v)))
                                      s))
                                  source
                                  params)
                        rest (reduce dissoc params keys)]
                    (if (empty? rest)
                      s
                      (str s "?" (str/join "&" (map (fn [[k v]]
                                                      (str (url/url-encode (name k)) "=" (url/url-encode (str v))))
                                                    rest)))))))
              (-parse-uri [this uri]
                (let [furl (url/url uri)
                      b (-> c-pattern
                            ;; in particular, we must remove the query params to use clout:
                            (clout/route-matches {:uri (:path furl)}))]
                  (if b
                    (merge b (into {} (map (fn [[k v]] [(keyword k) v]) (:query furl))))
                    false)))))

#?(:clj
   (defrecord ^:private Route [c-pattern]
     IRoutable 
     (-route-matches [_ request] (clout/route-matches c-pattern request))))

#?(:cljs
   (defn route [pattern]
     (Route. (clout/route-compile pattern))))

#_(defmacro defroute [name pattern]
  )
