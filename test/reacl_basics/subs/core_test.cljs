(ns reacl-basics.subs.core-test
  (:require [reacl-basics.subs.core :as core]
            [reacl-basics.subs.time :as time]
            [reacl-basics.actions.core :as actions]
            [reacl2.core :as reacl]
            [reacl2.test-util.alpha :as test-util]
            cljs.test)
  (:require-macros [cljs.test :refer (is deftest testing async)]))



(defn test-subscribe [sub]
  (let [msgs (atom [])
        sub-tester (reacl/class "sub-tester" this [sub at]
                                render (actions/action-handler
                                        (core/subscribe-to
                                         sub
                                         (fn [value]
                                           (swap! at conj value)
                                           "foo"))))
        ]
    (test-util/instantiate&mount sub-tester sub msgs)
    msgs))

(defn test-unsubscribe [sub]
  (let [msgs (atom [])
        
        unsub-tester (reacl/class "unsub-tester" this [sub at]
                                  render "42"
                                  component-will-mount
                                  (fn []
                                    (reacl/return :action (core/subscribe-action sub this
                                                                                 #(vector :id %)
                                                                                 #(vector :value %))))
                                  handle-message
                                  (fn [msg]
                                    (case (first msg)
                                      :id (reacl/return :action (core/unsubscribe-action sub (second msg)))
                                      :value (do (reset! at false)
                                                 (reacl/return)))))
        main (reacl/class "main" this [sub at]
                          render (actions/action-handler
                                  (unsub-tester sub at)))
        res (atom true)]
    (test-util/instantiate&mount main sub res)
    res))

;; TODO: better test subscribe-to, with updates, unmount, etc.

(defn wait [ms f]
  (js/window.setTimeout f ms))

(deftest concurrent-test
  (async done
         (let [s (core/concurrent (time/delay 21) (time/delay 42))]
           (is (= s
                  (core/concurrent (time/delay 21) (time/delay 42))))
           (let [res1 (test-subscribe s)
                 res2 (test-unsubscribe s)]
             (wait 10 (fn []
                        (is (= @res1
                               [nil [21 nil] [21 42]]))
                        (is @res2)
                        (done)))))))

(deftest serialized-test
  (async done
         (let [s (core/serialized (time/delay 21) (time/delay 42))]
           (is (= s
                  (core/serialized (time/delay 21) (time/delay 42))))
           (let [res1 (test-subscribe s)
                 res2 (test-unsubscribe s)]
             (wait 10 (fn []
                        (is (= @res1
                               [nil 21 42]))
                        (is @res2)
                        (done)))))))

(deftest void-test
  (is (= @(test-subscribe core/void)
         [nil]))
  (is @(test-unsubscribe core/void)))

(deftest sliding-window-test
  (async done
         (is (= (core/sliding-window (time/delay 21) 3)
                (core/sliding-window (time/delay 21) 3)))
         (let [sub (core/sliding-window (core/serialized (time/delay 21) (time/delay 22) (time/delay 23))
                                        2)
               res1 (test-subscribe sub)
               res2 (test-unsubscribe sub)]
           (wait 10
                 (fn []
                   (is (= @res1
                          [nil [nil 21] [21 22] [22 23]]))
                   (is @res2)
                   (done))))))

(deftest subs-handler-test
  (let [mk-msg #(vector :value %)
        values (atom [])
        inner (reacl/class "inner" this []
                           render "foo"
                           component-will-mount
                           (fn []
                             (reacl/return :action (core/update-subs this {(core/serialized (time/delay 21 1) (time/delay 22 1) (time/delay 23 1)
                                                                                            ;; will unsub before this:
                                                                                            (time/delay 24 5))
                                                                           mk-msg})))
                           handle-message
                           (fn [msg]
                             (assert (= :value (first msg)))
                             (swap! values conj (second msg))
                             (if (= (second msg) 23)
                               (reacl/return :action (core/clear-subs this))
                               (reacl/return))))
        c (test-util/instantiate&mount
           (reacl/class "outer" this []
                        render (actions/action-handler
                                (core/subs-handler (inner)))))]
    (async done
           (wait 20
                 (fn []
                   (is (= @values
                          [21 22 23]))
                   (done))))))
