(ns reacl-basics.forms
  "Functions and classes to build user input forms based on app-states
  and declarative client side validation."
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

(defn- update-attr [m caml-cased-key f & args]
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

(defn- set-value [attrs value]
  (assoc attrs :value value))

(c/defc ^:private form-value-element opt app-state [f attrs & content]
  (apply form-element opt app-state get-value set-value f attrs content))

(defn- get-checked [elem]
  (.-checked elem))

(defn- set-checked [attrs value]
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

(defrecord ^:private Cleanup [])

(reacl/defclass ^:private input-parsed this value [parse unparse restrict & [attrs]]
  ;; Note parse/unparse must not change during livecycle.
  local-state [state (let [s (unparse value)]
                       {:text s
                        :last-text s
                        :last-value value})]

  component-did-update
  (fn []
    (if (not= value (:last-value state))
      ;; parent changed value to something unrelated to the user's input - start over
      (reacl/return :local-state (let [s (unparse value)]
                                   {:text s
                                    :last-text s
                                    :last-value value}))
      (if (not= (:last-text state) (:text state))
        ;; user changed text, to...
        (let [p (parse (:text state))]
          (if (= p value)
            ;; same parsed value, - just remember the text
            (reacl/return :local-state (-> state
                                           (assoc :last-text (:text state))))
            ;; new, parseable value entered - publish that and remember text.
            (reacl/return :app-state p
                          :local-state (-> state
                                           (assoc :last-value p)
                                           (assoc :last-text (:text state))))))
        ;; nothing changed:
        (reacl/return))))

  ;; TODO: maybe the parent should have the option to force an update of the text? A special message? To be used in onblur for example.
  ;; or off a version with :text in app-state for that?!
  
  render
  (input-text #_(reacl/bind-locally this :text)
              (reacl/opt :reaction (reacl/reaction this ->Change))
              (:text state)
              (cond-> (or attrs {})
                (:cleanup-on-blur? attrs) (update-attr :onBlur (fn [prev]
                                                                 (fn [ev]
                                                                   (reacl/send-message! this (->Cleanup))
                                                                   (when prev (prev ev))
                                                                   nil)))))

  handle-message
  (fn [msg]
    (condp instance? msg
      Change (reacl/return :local-state (assoc state :text (restrict (:text state) (:value msg))))
      Cleanup (reacl/return :local-state (assoc state :text (unparse value))))))

(defn- parse-number [s]
  ;; Note: "" parses as NaN too
  (let [x (.parseFloat js/Number s)]
    (if (js/isNaN x)
      nil
      x)))

(defn- unparse-number [v]
  (if v (str v) ""))

(defn- unrestricted [old new]
  new)

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
  ;; This means, that our 'parse-number' will usually never see an invalid number representation (when :type is number)

  validate (assert (or (nil? value) (number? value)))

  render
  (input-parsed (reacl/opt :embed-app-state (fn [_ s] s))
                value
                parse-number
                unparse-number
                unrestricted
                (-> (or attrs {})
                    (update-attr :type #(or % "number")))))

#_(defn- parse-int [s]
  ;; Note: "" parses as NaN too
  (let [x (.parseInt js/Number s)]
    (if (js/isNaN x)
      nil
      x)))

#_(defn- unparse-int [v]
  (if v (str v) ""))

#_(defn restrict-int-str [s]
  (if-let [v (parse-int s)]
    (unparse-int v)
    s))

#_(reacl/defclass input-int this value [& [attrs]]
  validate (assert (or (nil? value) (integer? value)))
  
  render
  (input-parsed (reacl/opt :embed-app-state (fn [_ s] s))
                value
                parse-int
                unparse-int
                restrict-int-str
                (-> (or attrs {})
                    (update-attr :pattern #(or % #"\d*"))
                    #_(update-attr :pattern #(or % #"[0-9]*"))
                    #_(update-attr :step #(or % 1))
                    (update-attr :type #(or % "text")))))

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
   events `:onsubmit` and `:onreset` must be assigned functions taking
   an event and returning a `reacl/return` value."
  [attrs & content]
  ;; Note: often, one will have to set preventDefault in onSubmit.
  (apply adom/form attrs content))

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
