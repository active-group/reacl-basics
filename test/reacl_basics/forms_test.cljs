(ns reacl-basics.forms-test
  (:require [reacl-basics.forms :as forms]
            [reacl2.core :as reacl :include-macros true]
            [reacl2.dom :as dom]
            cljs.test
            [reacl2.test-util.beta :as tu]
            [reacl2.test-util.xpath :as xp :include-macros true]
            ["react-dom/test-utils" :as react-tu])
  (:require-macros [cljs.test :refer (is deftest testing async)]))

(deftest input-number-test
  (let [last-custom-validity (atom nil)
        current-validity (atom #js {"customError" ""})
        c (tu/env forms/input-number {:create-node-mock (fn [input]
                                                          #js {"setCustomValidity" (fn [v]
                                                                                     (reset! last-custom-validity v))
                                                               "validity" @current-validity}
                                                          )})

        input (fn [c]
                (xp/select-one (tu/get-component c) (xp/>> ** "input")))
        display (fn [c]
                  (.-value (.-props (input c))))
        ]

    (testing "can show ints and floats"
      (is (= (tu/mount! c 42) (reacl/return)))
      (is (= (display c) "42"))

      (is (= (tu/mount! c 42.2) (reacl/return)))
      (is (= (display c) "42.2")))

    (testing "can update to other numbers"
      (tu/mount! c 42)

      (is (= (tu/update! c 21) (reacl/return)))
      (is (= (display c) "21"))

      (is (= (tu/update! c 21.8) (reacl/return)))
      (is (= (display c) "21.8")))

    (testing "can react to valid user input"
      (tu/mount! c 42)

      (is (= (tu/invoke-callback! (input c) :onchange #js {:target #js {:value "42.9"}})
             (reacl/return :app-state 42.9))))

    (testing "does react to invalid user input with a nil value"
      (tu/mount! c 42)

      (is (= (tu/invoke-callback! (input c) :onchange #js {:target #js {:value "foobar"}})
             (reacl/return :app-state nil))))

    (testing "the user can correct himself in the invalid state"
      (tu/mount! c nil)

      (is (= (tu/invoke-callback! (input c) :onchange #js {:target #js {:value "foobar"}})
             (reacl/return))))

    (testing "does not change partial input"
      (tu/mount! c 21)

      ;; Note: if mounted with 42 instead, it only works, when the class sees local-state and app-state changes in a single update :-/
      ;; But outside of the test framework, that should usually be the case.
      (tu/invoke-callback! (input c) :onchange #js {:target #js {:value "021"}})
      (is (= (display c) "021")))
    
    ))

(reacl/defclass monitor-app-state this app-state [app-state-atom c & args]
  render (apply c (reacl/opt :reaction (reacl/pass-through-reaction this)) app-state
                args)
  component-did-update
  (fn []
    (reset! app-state-atom app-state)
    (reacl/return))
  component-did-mount
  (fn []
    (reset! app-state-atom app-state)
    (reacl/return))
  handle-message
  (fn [new-app-state]
    (reacl/return :app-state new-app-state)))

(deftest input-number-validation-dom-test
  ;; testing all validaton aspects requires a browser implementation of the input field.
  (if-let [document (and js/window (.-document js/window))]
    (let [c (.createElement document "div")
          
          update! (fn [c app-state-atom state & args]
                    (apply reacl/render-component c monitor-app-state state app-state-atom forms/input-number args))
          input (fn [c]
                  (loop [cs [c]]
                    (if (empty? cs)
                      nil
                      (if (instance? js/HTMLInputElement (first cs))
                        (first cs)
                        (recur (concat (rest cs) (when (instance? js/Element (first cs))
                                                   (array-seq (.-children (first cs))))))))))
          display (fn [c]
                    (.-value (input c)))

          enter! (fn [c text]
                   (let [elem (input c)]
                     ;; Note: 'ordinary' event don't seem to trigger a react event handler. Need React test utils:
                     (set! (.-value elem) text)
                     (.change (.-Simulate react-tu) elem)))]

      (let [app-state-atom (atom nil)
            comp (update! c app-state-atom 42 {:required true})]
        (testing "extra input is valid and preserved"
          (enter! c "021")
          (is (= (display c) "021"))
          (is (= @app-state-atom 21)))

        ;; Note: it seems it's impossible to bring the input field
        ;; into an 'invalid' state programatically - although the user
        ;; can :-/
        ;; but: value is empty and 'validity.badInput = true' then  (undocumented)
        
        (testing "validity is marked as a customError"
          (update! c app-state-atom -42 {:validity "Value must not be negative"})
          (is (.-customError (.-validity (input c))))))      
      
      )
    (js/console.warn "Test skipped, because no DOM implementation was found.")))
