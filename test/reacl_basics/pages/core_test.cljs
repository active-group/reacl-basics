(ns reacl-basics.pages.core-test
  (:require [reacl-basics.pages.core :as core]
            [reacl-basics.actions.core :as actions]
            [reacl-basics.pages.routes :as routes :include-macros true]
            [reacl-basics.pages.history :as history]
            [reacl2.core :as reacl :include-macros true]
            [reacl2.dom :as dom]
            [reacl2.test-util.alpha :as tu])
  (:require-macros [cljs.test :refer (is deftest testing async)]))

(defn doms-with-tag
  [comp tag-name]
  (into []
        (js/ReactTestUtils.scryRenderedDOMComponentsWithTag comp tag-name)))

(defn dom-content
  [comp]
  (.-textContent comp))

(defn snd [a b] b)

(deftest history-router-test
  (routes/clear-routes!)

  (routes/defroute home "/home")
  (routes/defroute person "/person/:id")

  (let [home-page (reacl/class "home" this app-state []
                               render (dom/div "Homepage"))
        person-page (reacl/class "person" this app-state [opt id]
                                 render (dom/div "Person: " id))

        pages {home home-page
               person (core/page person-page 'opt)}]

    (async done
           (let [hist-nav! (atom nil)
                 current-path (atom "/home")
                 test-history
                 (reify history/History
                   (push! [_ path]
                     (reset! current-path path))
                   (get-current [_]
                     @current-path)
                   (start! [_ nav-path! path-exists?]
                     (reset! hist-nav! nav-path!))
                   (stop! [_]
                     (reset! hist-nav! nil)))
        
                 main (reacl/class "main" this app-state []
                                   render
                                   (actions/action-handler
                                    (core/history-router (reacl/opt :embed-app-state snd) {}
                                                         test-history
                                                         pages)))

                 c (tu/instantiate&mount main {})]
             (is (= (map dom-content (doms-with-tag c "div"))
                    ["Homepage"]))
             ;; simulate a user click on an anchor:
             (js/window.setTimeout
              (fn []
                (@hist-nav! "/person/123")
                (is (= (map dom-content (doms-with-tag c "div"))
                       ["Person: 123"]))
                (done))
              0)))))
