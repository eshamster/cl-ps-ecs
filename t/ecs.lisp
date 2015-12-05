(in-package :cl-user)
(defpackage cl-ps-ecs-test.ecs
  (:use :cl
        :cl-ps-ecs
        :parenscript
        :ps-experiment
        :prove)
  (:import-from :cl-ps-ecs.ecs
                :clean-ecs-entities)
  (:import-from :ps-experiment-test.test-utils
                :prove-in-both))
(in-package :cl-ps-ecs-test.ecs)

(plan 2)

;; TOOD: push and restore the ps environment

(defstruct.ps+ (sample-entity (:include ecs-entity)) a)
(defstruct.ps+ not-entity a b)

(defmacro with-add-entity (&body body)
  `(unwind-protect
        (progn ,@body)
     (clean-ecs-entities)))

(subtest
    "Test entity funcs"
  (subtest
      "Test add-ecs-entity"
    (with-add-entity
      (prove-in-both (ok (let ((parent (make-sample-entity))
                               (child (make-sample-entity)))
                           (add-ecs-entity parent)
                           (add-ecs-entity child parent)
                           (eq (sample-entity-parent child) parent)))))
    (with-add-entity
      (prove-in-both (ok (let ((parent (make-sample-entity))
                               (child (make-sample-entity)))
                           (add-ecs-entity parent)
                           (add-ecs-entity child parent)
                           (eq (nth 0 (sample-entity-children parent)) child)))))
    (prove-in-both (is-error (add-ecs-entity (make-not-entity))
                             'type-error)))
  (subtest
      "Test process-all-entities"
    (with-add-entity
        (prove-in-both (is (let ((entity (make-sample-entity :a 1))
                                 (sum 0))
                             (add-ecs-entity entity)
                             (add-ecs-entity (make-sample-entity :a 2))
                             (add-ecs-entity (make-sample-entity :a 3) entity)
                             (add-ecs-entity (make-sample-entity :a 4) entity)
                             (process-all-entities (lambda (ent)
                                                     (incf sum (sample-entity-a ent))))
                             sum)
                           10)))))

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
