(ns reacl-basics.subs.server-test
  (:require [reacl-basics.subs.server :as server]
            [reacl-basics.subs.core-test :as core-test]
            [reacl-basics.actions.core :as action]
            [clojure.string :as string])
  (:require-macros [cljs.test :refer (is deftest testing async)]))

(defn wait-until [ms-base iter pred f]
  (if (zero? iter)
    (f)
    (core-test/wait ms-base
                    (fn []
                      (if (pred)
                        (f)
                        (wait-until (* ms-base 2) ;; exponential backoff.
                                    (dec iter)
                                    pred
                                    f))))))

(defn test-ajax-sub [sub f]
  (let [res1 (core-test/test-subscribe sub)]
    (wait-until 20 5
                (fn [] (some? (last @res1)))
                (fn []
                  (f (last @res1))))))

(defn existing-url []
  ;; the figwheel main test page, hopefully.
  (.-href (.-location js/window)))

(defn non-existing-url []
  ;; a (hopefully) wrong url to test.
  (str (existing-url)
       "_invalid"))

(deftest ajax-base-request-test
  (async done
         (test-ajax-sub (server/GET (existing-url) {})
                        (fn [[ok html]]
                          (is ok)
                          (is (string/starts-with? html "<!DOCTYPE html>"))
                          (done)))))

(deftest ajax-failed-request-test
  (async done
         (test-ajax-sub (server/GET (non-existing-url) {})
                        (fn [[ok response]]
                          (is (not ok))
                          (is (= (:status response)
                                 404))
                          (done)))))

(deftest ajax-abort-request-test
  (async done
         (let [res1 (core-test/test-subscribe (server/GET (existing-url) {}))]
           ;; wait a bit, then see there is not result
           (core-test/wait 50
                           (fn []
                             (is @res1)
                             (done))))))

(deftest ajax-request-test
  ;; Note: ajax-request has more differences that just how uri and method are specified
  (async done
         (test-ajax-sub (server/ajax-request {:uri (existing-url)
                                              :method :get
                                              :response-format (ajax.core/raw-response-format)})
                        (fn [[ok html]]
                          (is ok)
                          (is (string? html))
                          (is (string/starts-with? html "<!DOCTYPE html>"))
                          (done)))))
