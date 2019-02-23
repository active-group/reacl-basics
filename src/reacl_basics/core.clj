(ns reacl-basics.core
  (:require [reacl2.core :as reacl]
            [reacl-basics.utils :as u]
            [clojure.spec.alpha :as s]))

(defn- assoc? [m k v & more]
  (cond-> m
    (not (contains? m k)) (assoc k v)
    (not-empty more) (as-> $ (apply assoc? $ more))))

(defmacro ^:no-doc defn-pseudo [name docstring? params & body]
  `(def ~(vary-meta name
                    assoc?
                    :doc (when (string? docstring?) docstring?)
                    :arglists (let [p (if (string? docstring?)
                                        params
                                        docstring?)]
                                (if (list? p)
                                  p
                                  (list p))))
     (do ~@body)))

(defmacro ^:no-doc app-state-defc [opt app-state params body]
  `(with-opt-detector (fn [~opt ~app-state ~@params]
                        ~@body)))

(defmacro ^:no-doc no-app-state-defc [opt params body]
  `(with-opt-detector (fn [~opt ~@params]
                        ~@body)))


(s/def ::defc-args
  (s/cat :docstring (s/? string?)
         :opt simple-symbol?
         :app-state (s/? simple-symbol?)
         :params vector?
         :body (s/* any?)))

(def ^:no-doc parse-defc-args
  (u/spec-parser ::defc-args))

(defmacro defc
  "Defines a 'light-weight' class, i.e. a function that can be called like a reacl class.
The `opt` parameter will always be set, even if the caller did not specify one. Note that you should always pass `opt` and `app-state` as-is to some other class instantiation.

For example with the following definition:

```
(defc reverser opt app-state [& items]
  (list-view opt app-state (reverse items))
```

then `reverser` can be used just as if it would be an equivalent class.

Note that the macro can also be used without an app-state.
"
  [name & args]
  (let [m (parse-defc-args args)
        has-app-state? (contains? m :app-state)]
    `(def ~(vary-meta name assoc?
                      :doc (:docstring m)
                      :arglists `(~(:params m))
                      #_(if has-app-state?
                                  `([~(:opt m) ~(:app-state m) ~@(:params m)]
                                    [~(:app-state m) ~@(:params m)])
                                  `([~(:opt m) ~@(:params m)]
                                    [~@(:params m)])))
       ~(if has-app-state?
          `(app-state-defc ~(:opt m) ~(:app-state m) ~(:params m) ~(:body m))
          `(no-app-state-defc ~(:opt m) ~(:params m) ~(:body m))))))

(alter-meta! #'defc assoc :arglists '([name opt app-state? [params*] body]))

(s/def ::defn-dom-args
  (s/cat :docstring (s/? string?)
         :params vector?
         :body (s/* any?)))

(def ^:no-doc parse-defn-dom-args (u/spec-parser ::defn-dom-args))

(defmacro defn-dom
  "Like [[clojure.core/defn]] where the first argument will always be an
  attribute map, event if not called with one."
  [name & args]
  (let [m (parse-defn-dom-args args)]
    `(let [f# (fn ~(:params m) ~@(:body m))]
       (def ~(vary-meta name assoc?
                        :doc (:docstring m)
                        :arglists `([~@(rest (:params m))] ~(:params m)))
         (fn [& args#]
           (apply attrs-detector f args#))))))

(alter-meta! #'defn-dom assoc :arglists '([name docstring? [attrs params*] body]))

(defmacro defc-dom
  "Like [[defc]], where the first class argument will always be an
  attribte map, event if not called with one."
  [name & args]
  (let [m (parse-defc-args args)]
    (assert (not (s/invalid? m)) m)
    `(defc
       ~(vary-meta name assoc? :arglists `([~@(rest (:params m))] ~(:params m)))
       ~@(when (contains? m :docstring)
           [(:docstring m)])
       ~(:opt m)
       ~@(when (contains? m :app-state)
           [(:app-state m)])
       [& args#]
       (attrs-detector (fn [attrs# & rest#]
                         (apply (fn ~(:params m)
                                  ~@(:body m))
                                attrs# rest#))
                       args#))))

(alter-meta! #'defc-dom assoc :arglists '([name docstring? opt app-state? [attrs params*] body]))
