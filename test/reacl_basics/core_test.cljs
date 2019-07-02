(ns reacl-basics.core-test
  (:require [reacl-basics.core :as core :include-macros true]
            [reacl2.core :as reacl :include-macros true]
            [reacl2.dom :as dom]
            cljs.test
            [reacl2.test-util.alpha :as tu])
  (:require-macros [cljs.test :refer (is deftest testing async)]))

(deftest join-classes-test
  (is (= (core/join-classes "ab cd" nil "de" "" "fg")
         "ab cd de fg")))

(deftest merge-attributes-test
  (is (= (core/merge-attributes)
         {}))
  (is (= (core/merge-attributes {:key "b"})
         {:key "b"}))
  (is (= (core/merge-attributes {:key "b"} {:foo "baz"})
         {:key "b" :foo "baz"}))
  ;; deep merge style
  (is (= (core/merge-attributes {:style {:color "green" :font "Arial"}} {:style {:color "blue"}})
         {:style {:color "blue" :font "Arial"}}))
  ;; keep classes order
  (is (= (core/merge-attributes (array-map :class "a" :className "b") (array-map :className "c" :class "d"))
         {:class "a b c d"})))

(deftest defc-test
  (core/defc my-x opt state [arg1]
    (assert (= state ::state))
    (assert (= arg1 ::arg1))
    (dom/div))
  (my-x ::state ::arg1))

(deftest defc-dom-test
  ;; with app-state
  (core/defc-dom my-dom-x opt state [attrs arg1]
    (assert (= state ::state))
    (assert (dom/attributes? attrs))
    (assert (= arg1 ::arg1))
    (dom/div))
  (my-dom-x ::state ::arg1)
  (my-dom-x ::state {} ::arg1)

  ;; and without app-state
  (core/defc-dom my-dom-x-2 opt [attrs arg1]
    (assert (dom/attributes? attrs))
    (assert (= arg1 ::arg1))
    (dom/div))
  (my-dom-x-2 ::arg1)
  (my-dom-x-2 {} ::arg1))

(deftest defn-dom-test
  (core/defn-dom dom-x [attrs arg1]
    (assert (dom/attributes? attrs))
    (assert (= arg1 ::arg1))
    (dom/div))
  (dom-x ::arg1)
  (dom-x {} ::arg1))
