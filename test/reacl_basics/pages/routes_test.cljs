(ns reacl-basics.pages.routes-test
  (:require [reacl-basics.pages.routes :as routes :include-macros true])
  (:require-macros [cljs.test :refer (is deftest testing async)]))

(deftest parsing-test
  (let [rout0 (routes/route "/base/dir")]
    (is (= (routes/parse rout0 "/test")
           false))
    (is (= (routes/parse rout0 "/base/dir")
           {}))
    (is (= (routes/href rout0)
           "/base/dir")))
  
  (let [rout1 (routes/route "/base/:id")]
    (is (= (routes/parse rout1 "/test")
           false))
    (is (= (routes/parse rout1 "/base/123")
           {:id "123"})) ;; ???
    (is (= (routes/parse rout1 "/base/123?a=42")
           {:id "123" :a "42"}))

    (is (= (routes/href rout1 {:id "123"})
           "/base/123"))
    (is (= (routes/href rout1 {:id "123" :a "42"})
           "/base/123?a=42"))
    
    ))
