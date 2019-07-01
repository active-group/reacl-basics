(ns reacl-basics.bootstrap4-test
  (:require [reacl-basics.bootstrap4 :as b4]
            [reacl2.core :as reacl :include-macros true]
            [reacl2.dom :as dom]
            cljs.test
            [reacl2.test-util.alpha :as tu])
  (:require-macros [cljs.test :refer (is deftest testing async)]))

(defn classes [d] (.-className (.-props d)))

(deftest col-test
  (let [cl (fn [attrs] (classes (b4/col attrs)))]
    (is (= (cl {:class "x"})
           "col x"))
    (is (= (cl {:* 4 :class "x"})
           "col-4 x"))
    (is (= (cl {:* 4 :class "x"})
           "col-4 x"))
    (is (= (cl {:* 1 :xs 2 :sm 3 :md 4 :lg 5 :xl :auto})
           "col-1 col-xs-2 col-sm-3 col-md-4 col-lg-5 col-xl-auto"))
    (is (= (cl {:offset {:xs 2}
                :order {:* 4
                        :sm :last}})
           "col offset-xs-2 order-4 order-sm-last"))
    ))
