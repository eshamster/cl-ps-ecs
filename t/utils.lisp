(in-package :cl-user)
(defpackage cl-ps-ecs-test.utils
  (:use :cl
        :cl-ps-ecs.utils
        :parenscript
        :ps-experiment
        :prove)
  (:import-from :ps-experiment-test.test-utils
                :prove-in-both
                :is-list.ps+))
(in-package :cl-ps-ecs-test.utils)

(plan 1)

(defstruct.ps+ test1 a b)
(defstruct.ps+ (test2 (:include test1)) c)
(defstruct.ps+ test3 a b)

(subtest
    "Test includes-all-component-types"
  (prove-in-both (ok (includes-all-component-types
                      '(test1 test3)
                      (list (make-test2) (make-test3)))))
  (prove-in-both (ok (not (includes-all-component-types
                           '(test1 test3)
                           (list (make-test3))))))
  (prove-in-both (ok (not (includes-all-component-types
                           '(test3)
                           (list (make-test1) (make-test2))))))
  (prove-in-both (ok (includes-all-component-types
                      '()
                      (list (make-test1) (make-test2))))))

(finalize)
