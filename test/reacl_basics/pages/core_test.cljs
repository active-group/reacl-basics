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

(deftest router-with-pager-test
  (routes/clear-routes!)

  (routes/defroute home "/home")
  (routes/defroute person "/person/:id")

  (let [home-page (reacl/class "home" this app-state []
                                                render (dom/div "Homepage"))
        person-page (reacl/class "person" this app-state [id]
                                 render (dom/div "Person: " id))

        pages [(core/page home home-page)
               (core/page person person-page)]]

    (async done
           (let [hist-nav! (atom nil)
                 test-history
                 (reify history/History
                   (push! [this path]
                     ;; TODO: is html5 history sync or not?
                     (@hist-nav! path))
                   (start! [this nav-path! path-exists?]
                     (reset! hist-nav! nav-path!))
                   (stop! [this]
                     (reset! hist-nav! nil)))
        
                 main (reacl/class "main" this app-state []
                                   render
                                   (actions/action-handler
                                    (core/router-with-pager (reacl/opt :embed-app-state snd) {}
                                                            test-history
                                                            pages)))

                 c (tu/instantiate&mount main {})]
             (is (= (map dom-content (doms-with-tag c "div"))
                    ["Homepage"]))
             ;; simulate a user click on an anchor:
             (js/window.setTimeout
              (fn []
                (history/push! test-history "/person/123")
                (is (= (map dom-content (doms-with-tag c "div"))
                       ["Person: 123"]))
                (done))
              0)))))
