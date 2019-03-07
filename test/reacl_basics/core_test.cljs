(ns reacl-basics.core-test
  (:require [reacl-basics.core :as core]
            [reacl2.core :as reacl :include-macros true]
            [reacl2.dom :as dom]
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
  (is (= (core/merge-attributes {:style {:color "green" :font "Arial"}} {:style {:color "blue"}})
         {:style {:color "blue" :font "Arial"}}))
  )
