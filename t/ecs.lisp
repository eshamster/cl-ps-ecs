(in-package :cl-user)
(defpackage cl-ps-ecs-test.ecs
  (:use :cl
        :cl-ps-ecs
        :parenscript
        :ps-experiment
        :prove)
  (:import-from :cl-ps-ecs.ecs
                :clean-ecs-env)
  (:import-from :ps-experiment-test.test-utils
                :prove-in-both))
(in-package :cl-ps-ecs-test.ecs)

(plan 3)

;; TOOD: push and restore the ps environment

(defstruct.ps+ (sample-entity (:include ecs-entity)) a)
(defstruct.ps+ not-entity a b)

(defmacro with-modify-env (&body body)
  `(unwind-protect
        (progn ,@body)
     (clean-ecs-env)))

(defun.ps+ add-sample-entities-for-inherit (func)
  (let ((parent (make-sample-entity))
        (child (make-sample-entity)))
    (add-ecs-entity parent)
    (add-ecs-entity child parent)
    (funcall func parent child)))

(defun.ps+ add-sample-entities-for-loop ()
  (let ((entity (make-sample-entity :a 1)))
    (add-ecs-entity entity)
    (add-ecs-entity (make-sample-entity :a 2))
    (add-ecs-entity (make-sample-entity :a 3) entity)
    (add-ecs-entity (make-sample-entity :a 4) entity)))

(subtest
    "Test entity funcs"
  (subtest
      "Test add-ecs-entity"
    (with-modify-env
      (prove-in-both (ok (add-sample-entities-for-inherit
                          (lambda (parent child)
                            (eq (sample-entity-parent child) parent))))))
    (with-modify-env
      (prove-in-both (ok (add-sample-entities-for-inherit
                          (lambda (parent child)
                            (eq (nth 0 (sample-entity-children parent)) child))))))
    (locally
        (declaim #+sbcl (sb-ext:muffle-conditions sb-ext:compiler-note))
      (prove-in-both (is-error (add-ecs-entity (make-not-entity))
                               'type-error))))
  (subtest
      "Test do-ecs-entities"
    (with-modify-env
      (prove-in-both (is (let ((sum 0))
                           (add-sample-entities-for-loop)
                           (do-ecs-entities ent
                             (incf sum (sample-entity-a ent)))
                             sum)
                         10))))
  (subtest
      "Test find-a-entity"
    (with-modify-env
      (prove-in-both (is (progn (add-sample-entities-for-loop)
                                (sample-entity-a
                                 (find-a-entity (lambda (ent)
                                                  (and (sample-entity-p ent)
                                                       (= (sample-entity-a ent) 3))))))
                         3)))
    (prove-in-both (ok (not (find-a-entity
                             (lambda (ent) (eq ent 3))))))))

(defstruct.ps+ (cmp-parent (:include ecs-component)))
(defstruct.ps+ (cmp-child (:include cmp-parent)))
(defstruct.ps+ (cmp-independent (:include ecs-component)))

(defvar.ps+ *test-counter* 0)

(defstruct.ps+ (sys-test1 (:include ecs-system
                                    (target-component-types '(cmp-parent cmp-independent))
                                    (process (lambda (entity) (incf *test-counter*))))))

(subtest
    "Test system funcs"
  (subtest
      "Test register-ecs-system"
    (with-modify-env
      (prove-in-both (is (let ((ent1 (make-sample-entity))
                               (ent2 (make-sample-entity))
                               (ent3 (make-sample-entity)))
                           (add-ecs-component (make-cmp-child) ent1)
                           (add-ecs-component (make-cmp-independent) ent1)
                           (add-ecs-component (make-cmp-parent) ent2)
                           (add-ecs-component (make-cmp-independent) ent2)
                           (add-ecs-component (make-cmp-parent) ent3)
                           (add-ecs-entity ent1)
                           (add-ecs-entity ent2)
                           (add-ecs-entity ent3)
                           (register-ecs-system 'test (make-sys-test1))
                           (ecs-main)
                           *test-counter*)
                         2)))
    (setf *test-counter* 0)
    (prove-in-both (is-error (register-ecs-system (make-cmp-parent)
                                                  (make-sample-entity))
                             'type-error))
    (prove-in-both (is-error (register-ecs-system (make-sys-test1)
                                                  (make-cmp-parent))
                             'type-error))))

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
