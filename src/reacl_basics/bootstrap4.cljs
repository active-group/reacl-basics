(ns reacl-basics.bootstrap4
  (:require [reacl2.dom :as dom]
            [reacl2.core :as reacl :include-macros true]
            [reacl-basics.core :as c :include-macros true]))

;; -- Layout --------------------------------------

(c/defn-div container {:class "container"})

(c/defn-div container-fluid {:class "container-fluid"})

(c/defn-div row {:class "row"})
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

(c/defn-div w-100 {:class "w-100"})
(c/defn-div  w-75  {:class "w-75"})
(c/defn-div  w-50  {:class "w-50"})
(c/defn-div  w-25  {:class "w-25"})

;; TODO? display?
;; TODO? flexbox utils? (align-self...?)
;; TODO? padding and margin classes?
;; TODO? visibility

;; -- Alters --------------------------------------

(c/defn-div alert-primary   {:class "alert alert-primary" :role "alert"})
(c/defn-div alert-secondary {:class "alert alert-secondary" :role "alert"})
(c/defn-div alert-success   {:class "alert alert-success" :role "alert"})
(c/defn-div alert-danger    {:class "alert alert-danger" :role "alert"})
(c/defn-div alert-warning   {:class "alert alert-warning" :role "alert"})
(c/defn-div alert-info      {:class "alert alert-info" :role "alert"})
(c/defn-div alert-light     {:class "alert alert-light" :role "alert"})
(c/defn-div alert-dark      {:class "alert alert-dark" :role "alert"})

(c/defn-attr alert-heading-h1 dom/h1 {:class "alert-heading"})
(c/defn-attr alert-heading-h2 dom/h2 {:class "alert-heading"})
(c/defn-attr alert-heading-h3 dom/h3 {:class "alert-heading"})
(c/defn-attr alert-heading-h4 dom/h4 {:class "alert-heading"})
(c/defn-attr alert-heading-h5 dom/h5 {:class "alert-heading"})
(c/defn-attr alert-heading-h6 dom/h6 {:class "alert-heading"})

(c/defn-dom button-close [attrs]
  (dom/button (c/merge-attributes {:type "button" :class "close"} ;; should have a :aria-label
                                  attrs)
              (dom/span {:aria-hidden "true"} "\u00D7")))

;; -- Badges --------------------------------------

(c/defn-sa badge-primary   {:class "badge bade-primary"})
(c/defn-sa badge-secondary {:class "badge bade-secondary"})
(c/defn-sa badge-success   {:class "badge bade-success"})
(c/defn-sa badge-danger    {:class "badge bade-danger"})
(c/defn-sa badge-warning   {:class "badge bade-warning"})
(c/defn-sa badge-info      {:class "badge bade-info"})
(c/defn-sa badge-light     {:class "badge bade-light"})
(c/defn-sa badge-dark      {:class "badge bade-dark"})

(c/defn-attr sr-only dom/span {:class "sr-only"})

;; badge-pill ?

;; -- Breadcrumb --------------------------------------

;; TODO?

;; -- Button --------------------------------------

(c/defn-ba btn-primary   {:class "btn btn-primary"})
(c/defn-ba btn-secondary {:class "btn btn-secondary"})
(c/defn-ba btn-success   {:class "btn btn-success"})
(c/defn-ba btn-danger    {:class "btn btn-danger"})
(c/defn-ba btn-warning   {:class "btn btn-warning"})
(c/defn-ba btn-info      {:class "btn btn-info"})
(c/defn-ba btn-light     {:class "btn btn-light"})
(c/defn-ba btn-dark      {:class "btn btn-dark"})
(c/defn-ba btn-link      {:class "btn btn-link"})

(c/defn-ba btn-outline-primary   {:class "btn btn-outline-primary"})
(c/defn-ba btn-outline-secondary {:class "btn btn-outline-secondary"})
(c/defn-ba btn-outline-success   {:class "btn btn-outline-success"})
(c/defn-ba btn-outline-danger    {:class "btn btn-outline-danger"})
(c/defn-ba btn-outline-warning   {:class "btn btn-outline-warning"})
(c/defn-ba btn-outline-info      {:class "btn btn-outline-info"})
(c/defn-ba btn-outline-light     {:class "btn btn-outline-light"})
(c/defn-ba btn-outline-dark      {:class "btn btn-outline-dark"})
(c/defn-ba btn-outline-link      {:class "btn btn-outline-link"})

;; btn-block? btn-sizes? can be added as class...
;; active the same
;; disabled - different for button and a :-/ maybe worth a helper? but it's more likely a button if disabled programmatically.

;; TODO? checkbox and radio buttons? (as a reacl class?)

;; -- Button groups --------------------------------------

(c/defn-div btn-group {:class "btn-group" :role "group"})  ;; should have a aria-label or aria-labelled-by

(c/defn-div btn-group-vertical {:class "btn-group-vertical" :role "group"})

(c/defn-div btn-toolbar {:class "btn-toolbar" :role "toolbar"})

;; -- Cards --------------------------------------

(c/defn-div card {:class "card"})

(c/defn-div card-body {:class "card-body"})

(c/defn-attr card-title-h1 dom/h1 {:class "card-title"})
(c/defn-attr card-title-h2 dom/h2 {:class "card-title"})
(c/defn-attr card-title-h3 dom/h3 {:class "card-title"})
(c/defn-attr card-title-h4 dom/h4 {:class "card-title"})
(c/defn-attr card-title-h5 dom/h5 {:class "card-title"})
(c/defn-attr card-title-h6 dom/h6 {:class "card-title"})

(c/defn-attr card-subtitle-h1 dom/h1 {:class "card-subtitle"})
(c/defn-attr card-subtitle-h2 dom/h2 {:class "card-subtitle"})
(c/defn-attr card-subtitle-h3 dom/h3 {:class "card-subtitle"})
(c/defn-attr card-subtitle-h4 dom/h4 {:class "card-subtitle"})
(c/defn-attr card-subtitle-h5 dom/h5 {:class "card-subtitle"})
(c/defn-attr card-subtitle-h6 dom/h6 {:class "card-subtitle"})

(c/defn-attr card-link dom/a {:class "card-link"})
(c/defn-attr card-text dom/p {:class "card-text"})

(c/defn-attr card-img-top dom/img {:class "card-img-top"})

(c/defn-div card-header {:class "card-header"})

(c/defn-attr card-header-h1 dom/h1 {:class "card-header"})
(c/defn-attr card-header-h2 dom/h2 {:class "card-header"})
(c/defn-attr card-header-h3 dom/h3 {:class "card-header"})
(c/defn-attr card-header-h4 dom/h4 {:class "card-header"})
(c/defn-attr card-header-h5 dom/h5 {:class "card-header"})
(c/defn-attr card-header-h6 dom/h6 {:class "card-header"})

(c/defn-div card-footer {:class "card-footer"})

;; card-header-tabs, card-header-pills ?

(c/defn-attr card-img dom/img {:class "card-img"})
(c/defn-div card-img-overlay {:class "card-img-overlay"})

(c/defn-div card-group {:class "card-group"})
(c/defn-div card-deck {:class "card-deck"})
(c/defn-div card-columns {:class "card-columns"})

;; -- Carousel --------------------------------------

;; -- Collapse --------------------------------------

;; -- Dropdown --------------------------------------

;; TODO: do these need a 'button-grp' additionally - docu is unclear. Padding changes slightly then.
(c/defn-div dropdown {:class "dropdown"})
(c/defn-div dropup {:class "dropup"})
(c/defn-div dropright {:class "dropright"})
(c/defn-div dropleft {:class "dropleft"})

(c/defn-attr ^{:doc "A button that toggles the display of a popup menu.

   Add an :id to refer to it in a :aria-labelledby attribute of the dropdown-menu.
   Add color classes: btn-danger, etc.
   Add size classes: btn-lg, etc.
   Add an offset with :data-offset \"x,y\"
"}
  dropdown-toggle-button
  dom/button
  {:class "btn btn-primary dropdown-toggle"
   :type "button"
   :data-toggle "dropdown"
   :aria-haspopup "true"
   :aria-expanded "false"})

(c/defn-dom dropdown-toggle-split-button [attrs & [screen-reader-text]]
  (dropdown-toggle-button {:class "dropdown-toggle-split"}
                          (dom/span {:class "sr-only"} (or screen-reader-text "Toggle Dropdown"))))

(c/defn-div dropdown-menu {:class "dropdown-menu"}) ;; can add dropdown-menu-right, dropdown-menu-lg-right

(c/defn-ba dropdown-item {:class "dropdown-item"})

(c/defn-attr dropdown-item-text dom/span {:class "dropdown-item-text"})

(c/defn-div dropdown-divider {:class "dropdown-divider"})

(c/defn-attr dropdown-header-h1 dom/h1 {:class "dropdown-header"})
(c/defn-attr dropdown-header-h2 dom/h2 {:class "dropdown-header"})
(c/defn-attr dropdown-header-h3 dom/h3 {:class "dropdown-header"})
(c/defn-attr dropdown-header-h4 dom/h4 {:class "dropdown-header"})
(c/defn-attr dropdown-header-h5 dom/h5 {:class "dropdown-header"})
(c/defn-attr dropdown-header-h6 dom/h6 {:class "dropdown-header"})

;; -- Forms --------------------------------------

(c/defn-attr form dom/form {})
(c/defn-attr form-inline form {:class "form-inline"})

(c/defn-div form-group {:class "form-group"})
(c/defn-div form-group-row {:class "form-group row"})
(c/defn-div form-row {:class "form-row"})


(c/defn-attr input dom/input {:class "form-control"})
(c/defn-attr select dom/input {:class "form-control"})
(c/defn-attr textarea dom/input {:class "form-control"})

(c/defn-attr input-file dom/input {:class "form-control-file" :type "file"})
(c/defn-attr input-range dom/input {:class "form-control-range" :type "range"})

(c/defn-div form-check {:class "form-check"})
(c/defn-div form-check-inline {:class "form-check-inline"})

(c/defn-attr input-checkbox dom/input {:class "form-check-input" :type "checkbox"})
(c/defn-attr input-radio dom/input {:class "form-check-input" :type "radio"})

(c/defn-attr label dom/label {})
(c/defn-attr label-check dom/label {:class "form-check-label"})

(c/defn-attr col-label label {:class "col-form-label"})
(c/defn-attr col-label-check label-check {:class "col-form-label"})
(c/defn-attr fieldset dom/fieldset {:class "form-group"})
(c/defn-attr legend dom/legend {})
(c/defn-attr col-legend legend {:class "col-form-label"})

;; form-control-plaintext ?

;; -- Input group --------------------------------------
;; -- Jumpotron --------------------------------------
;; -- List group --------------------------------------
;; -- Media object --------------------------------------
;; -- Modal --------------------------------------

;; -- Navs --------------------------------------

(c/defn-attr nav-ul dom/ul {:class "nav"})

(c/defn-attr nav-ol dom/ol {:class "nav"})

(c/defn-attr nav-li dom/li {:class "nav-item"})

(c/defn-attr nav dom/nav {:class "nav" :role "navigation"}
  ;; add justify-content-center etc., flex-column
  ;; add nav-tabs, nav-pills
  ;; add nav-fill or nav-justified
  ;; more responsiveness: https://getbootstrap.com/docs/4.3/components/navs/#working-with-flex-utilities
  )

(c/defn-ba nav-link {:class "nav-link"})

;; -- Navbar --------------------------------------

(c/defn-attr navbar dom/nav {:class "navbar" :role "navigation"}
  ;; add navbar-light, navbar-dark
  ;; add bg-primary etc.
  ;; add fixed-top, fixed-bottom, sticky-top
  ;; add navbar-expand{-lg} to prevent collapsing.
  )

(c/defn-sa navbar-brand {:class "navbar-brand"})

(c/defn-dom navbar-toggler [attrs & content]
  (apply dom/button (c/merge-attributes {:class "navbar-toggler"
                                         :type "button"
                                         :data-toggle "collapse"
                                         :aria-expanded="false"}
                                        attrs)
         content))

(c/defn-div navbar-collapse {:class "collapse navbar-collapse"})

(c/defn-dom ^{:doc "Wraps the navbar items than can be collapsed on smaller display. Returns a list of dom elements!"} navbar-collapsable
  [attrs & content]
  (assert (:id attrs) "You must specify an :id")
  (list (navbar-toggler {:aria-label (:aria-label attrs)
                         :data-target (str "#" (:id attrs))
                         :aria-controls (str (:id attrs))})
        (apply navbar-collapse (dissoc attrs :aria-label)
               content)))

(c/defn-attr navbar-nav dom/nav {:class "navbar-nav" :role "navigation"})

(c/defn-attr navbar-nav-ul dom/ul {:class "navbar-nav"})

(c/defn-attr navbar-nav-ol dom/ol {:class "navbar-nav"})

(c/defn-attr navbar-text dom/span {:class "navbar-text"})


;; -- Pagination --------------------------------------
;; -- Popovers --------------------------------------
;; -- Progress --------------------------------------
;; -- Scrollspy --------------------------------------
;; -- Spinners --------------------------------------
;; -- Toasts --------------------------------------
;; -- Tooltips --------------------------------------
