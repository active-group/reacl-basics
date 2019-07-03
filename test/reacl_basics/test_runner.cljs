(ns reacl-basics.test-runner
  (:require
    [figwheel.main.testing :refer-macros [run-tests-async]]
    ;; require all the namespaces that have tests in them
    [reacl-basics.core-test]))

;; Note: http://localhost:9500/figwheel-extra-main/auto-testing
;; finds all tests anyway; figwheel just needs this file to get started.

(run-tests-async 10000)

