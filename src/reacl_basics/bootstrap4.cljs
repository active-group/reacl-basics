(ns reacl-basics.bootstrap4
  (:require [reacl2.dom :as dom]
            [reacl-basics.core :as c :include-macros true]))

;; -- Layout --------------------------------------

(c/defn-ca container {:class "container"})

(c/defn-ca container-fluid {:class "container-fluid"})

(c/defn-ca row {:class "row"})
;;(c/defn-cc no-gutters-row "row no-gutters")
;; TODO?? rows with "align-items..."? "justify-content-..."

(let [col-specials [:* :xs :sm :md :lg :xl]
      c-class (fn [b v]
                (str b (cond
                         (= :auto v) "-auto"
                         (and (>= v 1) (<= v 12)) (str "-" v)
                         ;; specials for :order:
                         (= :first v) "-first"
                         (= :last v) "-last"
                         :else (do (assert false (str "Don't understand:" v))
                                   ""))))
      c-spec (fn [prefix spec]
               (reduce-kv (fn [res k v]
                            (c/join-classes
                             res
                             (case k
                               :* (c-class prefix v)
                               (c-class (str prefix "-" (name k)) v))))
                          ""
                          spec))]
  (c/defn-dom col [attrs & content]
    (let [col-spec (select-keys attrs col-specials)
          specials (concat [:offset :order] col-specials)
          
          base (-> (if (empty? col-spec)
                     "col"
                     (c-spec "col" col-spec))
                   (c/join-classes (c-spec "offset" (:offset attrs)))
                   (c/join-classes (c-spec "order" (:order attrs))))]
      (apply dom/div (c/merge-attributes {:class base}
                                         (apply dissoc attrs specials))
             content))))

(c/defn-ca w-100 {:class "w-100"})

;; TODO? display?
;; TODO? flexbox utils? (align-self...?)
;; TODO? padding and margin classes?
;; TODO? visibility

;; -- Components --------------------------------------

(c/defn-ca alert-primary   {:class "alert alert-primary" :role "alert"})
(c/defn-ca alert-secondary {:class "alert alert-secondary" :role "alert"})
(c/defn-ca alert-success   {:class "alert alert-success" :role "alert"})
(c/defn-ca alert-danger    {:class "alert alert-danger" :role "alert"})
(c/defn-ca alert-warning   {:class "alert alert-warning" :role "alert"})
(c/defn-ca alert-info      {:class "alert alert-info" :role "alert"})
(c/defn-ca alert-light     {:class "alert alert-light" :role "alert"})
(c/defn-ca alert-dark      {:class "alert alert-dark" :role "alert"})

;; alerts also support h* elements with alert-heading in it.

