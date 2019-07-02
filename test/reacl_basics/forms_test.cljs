(ns reacl-basics.forms-test
  (:require [reacl-basics.forms :as forms]
            [reacl2.core :as reacl :include-macros true]
            [reacl2.dom :as dom]
            cljs.test
            [reacl2.test-util.beta :as tu]
            [reacl2.test-util.xpath :as xp :include-macros true])
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
      (tu/invoke-callback! (input c) :onchange #js {:target #js {:value "21."}})
      (is (= (display c) "21.")))
    
    ))
