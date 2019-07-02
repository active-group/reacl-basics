(ns reacl-basics.forms
  "Redefines some dom elements as classes that offer app-states
  for the content usually editable by a user in forms, and/or actions for the
  most common discrete user interactions."
  (:require [reacl2.dom :as dom]
            [reacl2.core :as reacl :include-macros true]
            [reacl-basics.adom :as adom]
            [reacl-basics.core :as c :include-macros true]
            [reacl-basics.classes :as cl]
            [clojure.string :as str]))

;; To set constraints on controls (https://www.w3.org/TR/html5/sec-forms.html#constraints-definitions)
;; - required=true attribute on any
;; - min, max, step attributes on value controls
;; - maxlength, minlength on textarea
;; - pattern attribute on value controls
;; - email, url types etc.?

;; additional attribute:
;; - :validity <message-string>   for custom validation

;; Elements: input, textarea select, (button,  output ?)

;; TODO https://www.w3schools.com/js/js_validation_api.asp
;; - setCustomValidity?
;; - checkValidity    (reportValidity)
;; - Events: onInvalid

(defn update-attr [m caml-cased-key f & args]
  ;; often caml-cased and lowercased are both possible :-/
  (let [l-key (keyword (str/lower-case (name caml-cased-key)))
        prev (or (l-key m) (caml-cased-key m))]
    (-> m
        (dissoc caml-cased-key)
        (assoc l-key (apply f prev args)))))

(defn- update-onchange [m f & args]
  (apply update-attr m :onChange f args))

(defrecord ^:private Change [value])

(defn- validate-form-element! [attrs ref]
  (let [elem (reacl/get-dom (or (:ref attrs) ref))]
    (if-let [msg (:validity attrs)]
      (.setCustomValidity elem msg)
      ;; apparently, validity was set before, so remove it
      (when (.-customError (.-validity elem))
        ;; Note: must be the empty string, not nil.
        (.setCustomValidity elem "")))))

(reacl/defclass ^:private form-element this value [get-value set-value f attrs & content]
  refs [it]

  component-did-mount
  (fn []
    (validate-form-element! attrs it)
    (reacl/return))

  component-did-update
  (fn []
    (validate-form-element! attrs it)
    (reacl/return))
  
  render
  (apply f (-> attrs
               (set-value value)
               (update :ref (fn [prev]
                              (or prev it)))
               (update-onchange (fn [prev]
                                  (fn [ev]
                                    (reacl/send-message! this (->Change (get-value (.-target ev))))
                                    nil))))
         content)

  handle-message
  (fn [msg]
    (condp instance? msg
      Change (reacl/return :app-state (:value msg))
      
      #_SetCustomValidity #_(let [elem (reacl/get-dom (or (:ref attrs) it))]
                              (.setCustomValidity elem (:error-msg msg))
                              (reacl/return)))))

(defn- get-value [elem]
  (.-value elem))

(defn set-value [attrs value]
  (assoc attrs :value value))

(c/defc ^:private form-value-element opt app-state [f attrs & content]
  (apply form-element opt app-state get-value set-value f attrs content))

(defn- get-checked [elem]
  (.-checked elem))

(defn set-checked [attrs value]
  (assoc attrs :checked value))

(c/defc ^:private form-checked-element opt app-state [f attrs & content]
  (apply form-element opt app-state get-checked set-checked f attrs content))

(c/defc ^:private input-value opt value [& args]
  (apply form-value-element opt value dom/input args))

(c/defc ^:private input-checked opt value [& args]
  (apply form-checked-element opt value dom/input args))

(c/defc-dom input-text
  "An app-state class that renders as a `dom/input` of `:type
  \"text\"`, with a `value` corresponding to its app-state.  Note that
  you can override the `:type` in the attrs, to get some predefined
  validators for the app-state string value. Typical other types are
  \"email\", \"password\", \"url\". Note that with type \"number\",
  you still get a string value as the app-state - use [[input-number]]
  for actual number values."
  opt value [attrs]
  (input-value opt value (merge {:type "text"} attrs)))

;; TODO: more input types?
;; "color" "date" "datetime-local" "email" "month" "number" "password" "search" "tel" "time" "url" "week"
;; "file" "hidden" "image" "range"
;; file input is tricky.
;; but then also make 'real' number inputs?

(defn- parse-number [s]
  ;; Note: "" parses as NaN too
  (let [x (.parseFloat js/Number s)]
    (if (js/isNaN x)
      nil
      x)))

(defn- unparse-number [v]
  (if v (str v) ""))

(reacl/defclass ^{:doc "An app-state class that renders as a
  `dom/input` to allow the user to edit the number corresponding to
  the app-state. The app-state may become nil, if the text the user
  currently entered is empty or not parsable as a number."}
  input-number this value [& [attrs]]
  
  ;; Note type can also be overriden by 'range' for example.

  ;; Note: an input element of type "number" behaves like this (in Chrome)
  ;; - the user cannot enter text that cannot be a number (like 'foobar')
  ;; - the user can enter text that might become a number (like 'e')
  ;; - if the current text cannot be parsed as a number, the 'value' is ""
  ;; - as long as it remains unparsable, no input/change event is triggered.
  ;; - if the current text can be parsed as a number, the 'value' is that text (like "021")
  ;; React does not change that.
  ;; This means, that out 'parse-number' will usually never see an invalid number representation (when :type is number)

  validate (assert (or (nil? value) (number? value)))

  local-state [state (let [s (unparse-number value)]
                       {:text s
                        :last-text s
                        :last-value value})]

  component-did-update
  (fn []
    (if (not= value (:last-value state))
      ;; parent changed value to something unrelated to the user's input - start over
      (reacl/return :local-state (let [s (unparse-number value)]
                                   {:text s
                                    :last-text s
                                    :last-value value}))
      (if (not= (:last-text state) (:text state))
        (let [p (parse-number (:text state))]
          (if (= p value)
            ;; same (but maybe with a leading 0 etc. - or sill not parsable) number entered. Just remember that
            (reacl/return :local-state (-> state
                                           (assoc :last-text (:text state))))
            (if (some? p)
              ;; new, parseable value entered - publish that.
              (reacl/return :app-state p
                            :local-state (-> state
                                             (assoc :last-value p)
                                             (assoc :last-text (:text state))))
              ;; new, non-parsable value entered, publish nil (if not done yet), but keep the text as is
              (if (nil? value)
                (reacl/return :local-state (-> state (assoc :last-text (:text state))))
                (reacl/return :app-state nil :local-state (-> state
                                                              (assoc :last-value nil)
                                                              (assoc :last-text (:text state))))))))
        ;; nothing changed:
        (reacl/return))))

  ;; TODO: maybe the parent should have the option to force an update of the text? A special message? To be used in onblur for example.
  ;; or off a version with :text in app-state for that?!
  
  render
  (input-text #_(reacl/bind-locally this :text)
              (reacl/opt :reaction (reacl/pass-through-reaction this))
              (:text state)
              (-> (or attrs {})
                  (update-attr :type #(or % "number"))))

  handle-message
  (fn [new-text]
    (reacl/return :local-state (assoc state :text new-text))))

(c/defc-dom input-checkbox
  "An app-state class with an optional `attrs` argument, which renders
  as an `dom/input` with `:type` \"checkbox\" representing the
  app-state."
  opt value [attrs]
  (input-checked opt value (merge {:type "checkbox"} attrs)))

(c/defc-dom input-radio
  "An app-state class with an optional `attrs` argument, which renders
  as an `dom/input` with `:type` \"radio\" representing the
  app-state."
  opt value [attrs]
  (input-checked opt value (merge {:type "radio"} attrs)))

(c/defn-dom form
  "A non-app-state class with an optional `attrs` and arbitrary
   `content` arguments which renders as a `dom/form` element. The
   events `:onsubmit` and `:onreset` must be assigned to zero
   arguments functions returning a `reacl/return` value."
  [attrs & content]
  (let [const-handler (fn [handler]
                        (when handler
                          ;; TODO: make it = with active-clojure/functions ?
                          (fn [ev]
                            ;; TODO: preventDefault might be (usually?) needed?
                            (handler))))]
    (apply adom/form (-> attrs
                         (update-attr :onSubmit const-handler)
                         (update-attr :onReset const-handler))
           content)))

(c/defn-dom button-submit
  "Returns a `dom/button` element with optional `attrs` and `content`, and `:type` \"submit\"."
  [attrs & content]
  (apply dom/button (merge {:type "submit"} attrs) content))

(c/defn-dom button-reset
  "Returns a `dom/button` element with optional `attrs` and `content`, and `:type` \"reset\"."
  [attrs & content]
  (apply dom/button (merge {:type "reset"} attrs) content))

(c/defc-dom textarea
  "An app-state class with an optional `attrs` argument, which renders
  as a `dom/textarea` with the app-state as the editable content."
  opt value [attrs]
  (form-value-element opt value dom/textarea attrs))

(c/defc-dom select
  "An app-state class with an optional `attrs` argument and
  arbitrary `content` elements, which renders as a `dom/select` where
  the `:value` attribute represents the app-state."
  opt value [attrs & content]
  (apply form-value-element opt value dom/select attrs content))
