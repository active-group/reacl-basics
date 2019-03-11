(ns reacl-basics.pages.ring
  (:require [reacl-basics.pages.routes :as routes]))

(defn wrap-client-fn-routes
  ([handler client-fn]
   (wrap-client-fn-routes handler @routes/defined-routes client-fn))
  ([handler routes client-fn]
   (fn [request]
     (if-let [route (first (filter some? (map #(routes/route-matches % request)
                                              routes)))]
       ;; TODO: really call client-fn with route?
       (apply client-fn route (routes/route-matches route request))
       (handler request)))))

(defn wrap-client-routes
  ([handler client]
   (wrap-client-fn-routes handler (constantly client)))
  ([handler routes client]
   (wrap-client-fn-routes handler routes (constantly client))))
