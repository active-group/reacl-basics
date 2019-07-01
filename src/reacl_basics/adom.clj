(ns ^{:doc "Supporting macros for Reacl's DOM library."}
  reacl-basics.adom)

(defmacro ^:no-doc defdom
  "Internal macro for constructing DOM-construction wrappers."
  [n]
  `(def ~(vary-meta n assoc
                    :doc (str "Returns a dom element corresponding to a `" n "` tag. The `attrs` argument is an optional map of attributes. The remaining `children` arguments must be other elements or strings.")
                    :arglists '([attrs & children] [& children]))
     (adom-function ~(name n))))
