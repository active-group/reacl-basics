(ns reacl-basics.dom
  "Redefines the some dom elements as classes that offer app-states
  for the content usually editable by a user, and/or actions for the
  most common discrete user interactions."
  (:require [reacl2.dom :as rdom]
            [reacl-basics.core :as c :include-macros true]
            [reacl-basics.classes :as cl]))

(c/defn-pseudo button
  "A non-app-state class that renders as a `dom/button` with the given
  `content`, and emits `true` as an action when clicked. It also sets `:type` to \"button\"."
  ([& content] [attrs & content])
  (cl/input-class "button"
                  (fn [attrs] (rdom/button (merge {:type "button"} attrs)))
                  :onclick (constantly true)))

(c/defn-pseudo input-value
  "An app-state class that renders as a `dom/input` with a `value` corresponding to its app-state."
  ([] [attrs])
  (cl/interactive-value-class "input-value" rdom/input))

(c/defc-dom input-text
  "An app-state class that renders as a `dom/input` with `:type` \"text\", with a `value` corresponding to its app-state."
  opt value [attrs]
  (input-value opt value
               (merge {:type "text"} attrs)))

;; TODO: more input types?
;; file input is tricky.

(c/defn-pseudo input-checkbox
  "An app-state class with an optional `attrs` argument, which renders
  as an `dom/input` with `:type` \"checkbox\" representing the
  app-state."
  ([] [attrs])
  (cl/interactive-checked-class "input-checkbox" "checkbox" rdom/input))

(c/defn-pseudo input-radio
  "An app-state class with an optional `attrs` argument, which renders
  as an `dom/input` with `:type` \"radio\" representing the
  app-state."
  ([] [attrs])
  (cl/interactive-checked-class "input-radio" "radio" rdom/input))

(c/defn-pseudo form
  "A non-app-state class with an optional `attrs` and arbitrary
   `content` arguments which renders as a `dom/form` element, and
   which will emit `true` as an action when submitted, and `false`
   when reset."
  ([& content] [attrs & content])
  (cl/input-class ""
                  rdom/form
                  :onsubmit (constantly true)
                  :onreset (constantly false)))

(c/defn-dom button-submit
  "Returns a `dom/button` element with optional `attrs` and `content`, and `:type` \"submit\"."
  [attrs & content]
  (apply rdom/button (merge {:type "submit"} attrs) content))

(c/defn-dom button-reset
  "Returns a `dom/button` element with optional `attrs` and `content`, and `:type` \"reset\"."
  [attrs & content]
  (apply rdom/button (merge {:type "reset"} attrs) content))

(c/defn-pseudo textarea
  "An app-state class with an optional `attrs` argument, which renders
  as a `dom/textarea` with the app-state as the editable content."
  ([] [attrs])
  (cl/interactive-value-class "textarea" rdom/text-area))

(c/defn-pseudo select
  "A non-app-state class with an optional `attrs` argument and
  arbitrary `content` elements, which renders as a `dom/select` where
  the `:value` attribute represents the app-state."
  ([& content] [attrs & content])
  (cl/interactive-value-class "select" rdom/select))
