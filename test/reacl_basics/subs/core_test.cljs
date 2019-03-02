(ns reacl-basics.subs.core-test
  (:require [reacl-basics.subs.core :as core]
            [reacl-basics.subs.time :as time]
            [reacl-basics.actions.core :as actions]
            [reacl2.core :as reacl]
            [reacl2.test-util.alpha :as test-util]
            cljs.test)
  (:require-macros [cljs.test :refer (is deftest testing async)]))

(reacl/defclass tester this [sub at]
  render (actions/action-handler
          (core/subscribe-to
           sub
           (fn [value]
             (swap! at conj value)
             "foo"))))

(defn test-subscribe [sub]
  (let [msgs (atom [])]
    (test-util/instantiate&mount tester sub msgs)
    msgs))

;; TODO: better test subscribe-to, with updates, unmount, etc.

(deftest const-test
  (is (= (core/const 42)
         (core/const 42)))
  (is (= @(test-subscribe (core/const 42))
         [nil 42])))

(deftest concurrent-test
  (is (= (core/concurrent (core/const 21) (core/const 42))
         (core/concurrent (core/const 21) (core/const 42))))
  (is (= @(test-subscribe (core/concurrent (core/const 21) (core/const 42)))
         [nil [21 42]])))

(deftest sequential-test
  (async done
         (is (= (core/sequential (core/const 21) (core/const 42))
                (core/sequential (core/const 21) (core/const 42))))
         (let [res (test-subscribe (core/sequential (core/const 21) (time/delay 22 1)))]
           (js/window.setTimeout (fn []
                                   (is (= @res
                                          [nil 21 22]))
                                   (done))
                                 10))))

(deftest void-test
  (is (= @(test-subscribe core/void)
         [nil])))

(deftest sliding-window-test
  (async done
         (is (= (core/sliding-window (core/const 21) 3)
                (core/sliding-window (core/const 21) 3)))
         (let [res (test-subscribe (core/sliding-window (core/sequential (core/const 21) (time/delay 22 1) (time/delay 23 2))
                                                        2))]
           (js/window.setTimeout (fn []
                                   (is (= @res
                                          [nil [nil 21] [21 22] [22 23]]))
                                   (done))
                                 10))))

