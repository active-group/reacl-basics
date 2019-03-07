(ns reacl-basics.pages.routes-test
  (:require [reacl-basics.pages.routes :as routes :include-macros true])
  (:require-macros [cljs.test :refer (is deftest testing async)]))

(deftest parsing-test
  (let [rout0 (routes/route "/base/dir")]
    (is (not (routes/parse rout0 "/test")))
    (is (= (routes/parse rout0 "/base/dir")
           []))
    (is (= (routes/href rout0)
           "/base/dir")))
  
  (let [rout1 (routes/route "/base/:id")]
    (is (not (routes/parse rout1 "/test")))
    (is (= (routes/parse rout1 "/base/123")
           ["123"]))
    (is (= (routes/parse rout1 "/base/123?a=42")
           ["123" {:a "42"}]))

    (is (= (routes/href rout1 "123")
           "/base/123"))
    (is (= (routes/href rout1 "123" {:a "42"})
           "/base/123?a=42")))
  
  (let [rout1 (routes/route "/base/:id/:name")]
    (is (= (routes/parse rout1 "/base/123/def")
           ["123" "def"]))
    (is (= (routes/parse rout1 "/base/123/def?a=42")
           ["123" "def" {:a "42"}]))

    (is (= (routes/href rout1 "123" "def")
           "/base/123/def"))
    (is (= (routes/href rout1 "123" "def" {:a "42"})
           "/base/123/def?a=42")))
  )

(deftest defroute-test
  (routes/clear-routes!)
  (routes/defroute test-r "/abc/:id")
  
  (is (= (count @routes/routes)
         1))
  (is (routes/routable? test-r))
  (is (= (routes/href test-r "123")
         "/abc/123")))
