(ns reacl-basics.actions.core-test
  (:require [reacl-basics.actions.core :as core]
            [reacl2.core :as reacl]
            [reacl2.test-util.alpha :as test-util])
  (:require-macros [cljs.test :refer (is deftest testing async)]))

(deftest action-test
  (is (= (core/action + 42)
         (core/action + 42)))
  (is (not= (core/action + 42)
            (core/action + 21))))

(deftest action?-test
  (is (core/action? (core/action + 42)))
  (is (not (core/action? {}))))

(deftest execute-test
  (is (= (core/execute! (core/action (fn [state] (reacl/return :app-state state))) ::state)
         (reacl/return :app-state ::state))))

(deftest nothing-test
  (is (= (core/execute! core/nothing 42)
         (reacl/return))))

(deftest external-test
  (let [f (fn [at] (swap! at * 2))
        at (atom 1)]
    (is (= (core/external f at)
           (core/external f at)))
    (is (not= (core/external f at)
              (core/external f nil)))
    
    (core/execute! (core/external f at) 42)
    (is (= @at 2))))

(deftest comp-test
  (let [f (fn [at] (swap! at * 2))
        at1 (atom 1)
        a1 (core/external f at1)
        at2 (atom 3)
        a2 (core/external f at2)]
    
    (is (= (core/comp a1 a2)
           (core/comp a1 a2)))
    (is (not= (core/comp a1 a2)
              (core/comp a2 a1)))
    
    (core/execute! (core/comp a1 a2) 42)
    (is (= @at1 2))
    (is (= @at2 6))

    (let [at (atom [])]
      (core/execute! (core/comp (core/external (fn [] (swap! at conj ::m1)))
                                (core/external (fn [] (swap! at conj ::m2)))) 42)
      (is (= @at
             [::m1 ::m2])))))

(deftest message-test
  (is (= (core/message 't 'm)
         (core/message 't 'm)))

  (is (= (core/execute! (core/message 't 'm) 42)
         (reacl/return :message ['t 'm]))))

(deftest async-messages-test
  (async done
         (let [at (atom [])
               tgt (test-util/instantiate&mount (reacl/class "tgt" this []
                                                             render "foo"
                                                             handle-message (fn [msg]
                                                                              (swap! at conj msg)
                                                                              (reacl/return))))
               later (fn [send! [m1 m2]]
                       (js/window.setTimeout (fn [] (send! m2)) 1)
                       m1)]
           (is (= (core/async-messages 't later [::m1 ::m2])
                  (core/async-messages 't later [::m1 ::m2])))

           (is (= (core/execute! (core/async-messages tgt later [::m1 ::m2]) 'state)
                  (reacl/return :message [tgt ::m1])))
           (js/window.setTimeout (fn []
                                   (is (= @at [::m2]))
                                   (done)) 10))))

(deftest action-handler-test
  (let [at (atom [])
        tgt (reacl/class "tgt" this []
                         render "foo"
                         handle-message (fn [msg]
                                          (if (= 1 msg)
                                            (reacl/return :action (core/message this 2))
                                            (do (swap! at conj msg)
                                                (reacl/return)))))
        main (test-util/instantiate&mount (reacl/class "main" this []
                                                       render (core/action-handler (tgt))))]
    (test-util/send-message! (js/ReactTestUtils.findRenderedComponentWithType main (reacl/react-class tgt))
                             1)
    (is (= @at [2]))))
