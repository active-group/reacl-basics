(ns reacl-basics.core
  (:require [reacl2.core :as reacl]
            [reacl-basics.utils :as u]))

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
  `(with-opt-detector true (fn [~opt ~app-state ~@params]
                             ~@body)))

(defmacro ^:no-doc no-app-state-defc [opt params body]
  `(with-opt-detector false (fn [~opt ~@params]
                              ~@body)))


(defn- p-first? [pred args & [default]]
  (if (and (not (empty? args)) (pred (first args)))
    [(first args) (rest args)]
    [default args]))

(defn- p-first [pred args msg]
  (let [r (p-first? pred args ::none)]
    (if (= r ::none)
      (throw (ex-info (str "Parse error, expected a " msg ", but got " (pr-str (first args))) {:form (first args)}))
      r)))

(defn- parse-defc-args [args]
  (let [[docstring? args] (p-first? string? args)
        ;; TODO: do we want to change this now with bindings?
        [opt args] (p-first simple-symbol? args "symbol")
        [app-state? args] (p-first? simple-symbol? args)
        [params args] (p-first vector? args "vector")
        body args]
    (cond-> {:opt opt
             :params params
             :body body}
      docstring? (assoc :docstring docstring?)
      app-state? (assoc :app-state app-state?))))

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

(defn- parse-defn-dom-args [args]
  (let [[docstring? args] (p-first? string? args)
        [params args] (p-first vector? args "vector")
        body args]
    (cond-> {:params params
             :body body}
      docstring? (assoc :docstring docstring?))))

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
           (apply attrs-detector f# args#))))))

(alter-meta! #'defn-dom assoc :arglists '([name docstring? [attrs params*] body]))

(defmacro defc-dom
  "Like [[defc]], where the first class argument will always be an
  attribte map, event if not called with one."
  [name & args]
  (let [m (parse-defc-args args)]
    `(defc
       ~(vary-meta name assoc? :arglists `([~@(rest (:params m))] ~(:params m)))
       ~@(when (contains? m :docstring)
           [(:docstring m)])
       ~(:opt m)
       ~@(when (contains? m :app-state)
           [(:app-state m)])
       [& args#]
       (apply attrs-detector (fn [attrs# & rest#]
                               (apply (fn ~(:params m)
                                        ~@(:body m))
                                      attrs# rest#))
              args#))))

(alter-meta! #'defc-dom assoc :arglists '([name docstring? opt app-state? [attrs params*] body]))

(defmacro ^:no-doc defn-attr [name dom-f pre]
  ;; defines a simple span container, with some attributes preset.
  `(let [attr-pre# ~pre]
     (defn-dom ~(vary-meta name assoc?
                           :arglists '([attrs & content] [& content]))
       [attrs# & content#]
       (apply ~dom-f (reacl-basics.core/merge-attributes attr-pre# attrs#) content#))))

(defmacro ^:no-doc defn-div [name pre]
  ;; defines a simple div container, with some attributes preset.
  `(defn-attr ~(vary-meta name assoc?
                          :doc (str "Like [[reacl.dom/div]] with preset attributes " (pr-str pre) ""))
     reacl2.dom/div ~pre))

(defmacro ^:no-doc defn-sa [name pre]
  ;; defines a simple span or anchor container, with some attributes preset.
  `(let [sa-f# (fn [at# & cont#]
                 (apply (if (contains? at# :href)
                          reacl2.dom/a
                          reacl2.dom/span)
                        at# cont#))]
     (defn-attr ~(vary-meta name assoc?
                            :doc (str "Like [[reacl.dom/span]], or like [[reacl.dom/a]] if attributes contains :href, and with preset attributes " (pr-str pre) ""))
       sa-f#
       ~pre)))

(defmacro ^:no-doc defn-ba [name pre]
  ;; defines a simple button or anchor container, with some attributes preset.
  ;; Unifies :disabled true
  `(let [anchor-disabled# {:class "disabled" :tabindex -1 :aria-disabled "true"}
         ba-f# (fn [at# & cont#]
                 (if (contains? at# :href)
                   (apply reacl2.dom/a (merge-attributes (cond-> {:role "button"}
                                                           (:disabled at#) (merge anchor-disabled#))
                                                         (dissoc at# :disabled))
                          cont#)
                   (apply reacl2.dom/button (merge-attributes {:type "button"} at#) cont#)))]
     (defn-attr ~(vary-meta name assoc?
                            :doc (str "Like [[reacl.dom/button]], or like [[reacl.dom/a]] if attributes contains :href, and with preset attributes " (pr-str pre) ""))
       ba-f#
       ~pre)))
