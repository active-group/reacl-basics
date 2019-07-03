(ns reacl-basics.bootstrap4-test
  (:require [reacl-basics.bootstrap4 :as b4]
            [reacl2.core :as reacl :include-macros true]
            [reacl2.dom :as dom]
            cljs.test
            [reacl2.test-util.alpha :as tu])
  (:require-macros [cljs.test :refer (is deftest testing async)]))

(defn classes [d] (.-className (.-props d)))

(defn destruct-dom [d]
  (cond
    (aget d "type")
    (let [props (js/Object.assign #js {} (aget d "props"))
          children (aget props "children")]
      (js-delete props "children")
      (apply list (symbol (str (aget d "type"))) (js->clj props :keywordize-keys true)
             (cond
               (string? children) [children]
               :else (map destruct-dom (and children (array-seq children))))))

    :else
    d))

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

#_(deftest labeled-test
  ;; simple group with a labelel input:
  (is (= (destruct-dom (b4/labeled (b4/input {:id "1"})
                                   (b4/label "Foobar")))
         '(div {:className "form-group"}
               (label {:htmlFor "1" :className ""} "Foobar")
               (input {:className "form-control" :id "1"}))))
  ;; checkbox, with different classes (and flipped order)
  (is (= (destruct-dom (b4/labeled (b4/input-checkbox {:id "1"})
                                   (b4/label "Foobar")))
         '(div {:className "form-check"}
               (input {:type "checkbox" :className "form-check-input" :id "1"})
               (label {:className "form-check-label" :htmlFor "1"} "Foobar"))))
  ;; or as radio, and optionally inline:
  (is (= (destruct-dom (b4/labeled {:inline? true}
                                   (b4/input-radio {:id "1"})
                                   (b4/label "Foobar")))
         '(div {:className "form-check-inline"}
               (input {:type "radio" :className "form-check-input" :id "1"})
               (label {:className "form-check-label" :htmlFor "1"} "Foobar")))))

(deftest nav-link-test
  (testing "buttons with onclick"
    (let [f (fn [_] "test")]
      (is (= (destruct-dom (b4/nav-link {:onclick f}))
             `(~'button {:type "button" :className "nav-link" :onClick ~f})))
      (is (= (destruct-dom (b4/nav-link {:disabled true :onclick f}))
             `(~'button {:type "button" :disabled true :className "nav-link" :onClick ~f}))))
    )
  (testing "anchors with href - unified :disabled attribute"
    (is (= (destruct-dom (b4/nav-link {:href "test"}))
           '(a {:role "button" :className "nav-link" :href "test"})))
    (is (= (destruct-dom (b4/nav-link {:disabled true :href "test"}))
           '(a {:role "button" :tabindex -1 :aria-disabled "true" :className "disabled nav-link" :href "test"}))))
  )
