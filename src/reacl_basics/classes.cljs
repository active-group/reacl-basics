(ns reacl-basics.classes
  "Functions and macros that create classes with some typical properties and features."
  (:require [reacl2.core :as reacl :include-macros true]
            [reacl2.dom :as dom]
            [reacl-basics.core :as core]
            [reacl-basics.utils :as u]))

(defn- parse-dom-varg [args]
  (if (dom/attributes? (first args))
    {:attrs (first args)
     :content (rest args)}
    {:attrs {}
     :content args}))

(defn- events->messages [target mp]
  (into {}
        (map (fn [[event action-f]]
               [event (fn [e]
                        (reacl/send-message! target (action-f e)))])
             mp)))

(defn- action-message-handler [msg]
  (if (some? msg)
    (reacl/return :action msg)
    (reacl/return)))

#_(defn input-class
  "Returns a non-app-state class with an optional `attrs` argument, and arbitrary more arguments.
   It renders as `(render attrs & args)`, where the `attrs` argument
  will contain event handlers for each additional event and function
  arguments to the macro, which will be called with the correspondig
  event object, and then cause the created component to return an
  action value as returned by that function, unless it's `nil`."
  [name render & event-actions]
  (let [event-actions (apply array-map event-actions)]
    (reacl/class name this [& args]
                 
                 local-state [consts {:event-attrs (events->messages this event-actions)}] ;; -> consts? methods?
                 
                 render (let [m (parse-dom-varg args)]
                          (apply render (merge (:attrs m) (:event-attrs consts))
                                 (:content m)))
                 
                 handle-message action-message-handler)))

(defrecord ^:no-doc Value [v])

(defn- action-and-value-message-handler [msg]
  (if (instance? Value msg)
    (reacl/return :app-state (:v msg))
    (action-message-handler msg)))

(defn interactive-class
  "Returns an app-state class with an optional `attrs` argument and arbitrary more arguments.
   It renders as `(render attrs value & more)`, where `value` will be
  current app-state of the component and the `attrs` argument will
  contain `value-event` with an event-handler that calls
  `value-extractor` on the event object and uses the result as the new
  app-state. Additional event and function arguments to the macro will
  also be preset in `attrs` with event-handlers that call those
  function on the correspondig event object, and then cause the
  created component to return an action value as returned by that
  function, unless it's `nil`."
  [name render value-event value-extractor & event-actions]
  (let [event-actions (apply array-map event-actions)]
    (reacl/class name this value [& args]

                 ;; TODO: use consts, methods when available.
                 local-state [consts {:event-attrs (events->messages this event-actions)
                                      :value-change-attrs {value-event (fn [e] (reacl/send-message! this (Value. (value-extractor e))))}}]
                 
                 render (let [m (parse-dom-varg args)]
                          (apply render (merge (:attrs m) (:value-change-attrs consts) (:event-attrs consts))
                                 value
                                 (:content m)))
                 
                 handle-message action-and-value-message-handler)))

(defn interactive-value-class
  "Returns an app-state class with an optional `attrs` and more arguments,
  that renders as `(render attrs & more)`. An `:onchange` and a `:value`
  attribute will be preset in `attrs` that reflect the current
  app-state, resp. cause an update of the app-state to the `.value`
  property of the rendered element."
  [name render]
  (interactive-class name
                     (fn [attrs value] (render (assoc attrs :value value)))
                     :onchange #(.-value (.-target %))))

(defn interactive-checked-class
  "Returns an app-state class with an optional `attrs` argument and
  that renders as `(render attrs)`. The `:type` attribute, and an
  `:onchange` and a `:checked` attribute will be preset in `attrs` that
  reflect the current app-state, resp. cause an update of the
  app-state to the `.checked` property of the rendered element."
  [name type render]
  (interactive-class name
                     (fn [attrs value] (render (merge {:type type}
                                                      attrs
                                                      {:checked value})))
                     :onchange #(.-checked (.-target %))))

