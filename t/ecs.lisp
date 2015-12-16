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
                :prove-in-both
                :is-list.ps+))
(in-package :cl-ps-ecs-test.ecs)

(plan 4)

;; TOOD: push and restore the ps environment

;; ---- Definitions for test ---- ;;

;; - values and structs - ;;

(defstruct.ps+ (cmp-parent (:include ecs-component)))
(defstruct.ps+ (cmp-child (:include cmp-parent)))
(defstruct.ps+ (cmp-independent (:include ecs-component)))

(defvar.ps+ *test-counter* 0)

(defstruct.ps+ (sys-test1 (:include ecs-system
                                    (target-component-types '(cmp-parent cmp-independent))
                                    (process (lambda (entity) (incf *test-counter*))))))

(defun.ps+ add-components-for-sys-test1 (entity)
  (add-ecs-component (make-cmp-parent) entity)
  (add-ecs-component (make-cmp-independent) entity))

(defstruct.ps+ (sample-entity (:include ecs-entity)) a)
(defstruct.ps+ not-entity a b)

;; - funcs and macros - ;;

(defmacro with-modify-env (&body body)
  `(unwind-protect
        (progn ,@body)
     (progn (clean-ecs-env)
            (setf *test-counter* 0))))

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

;; ---- Start test ---- ;;

(subtest
    "Test component funcs"
  (subtest
      "Test add-ecs-component" 
    (with-modify-env
      (prove-in-both (is (let ((ent1 (make-sample-entity))
                               (ent2 (make-sample-entity))
                               (ent3 (make-sample-entity)))
                           (register-ecs-system "sys1" (make-sys-test1))
                           ;; registered to sys1
                           (add-ecs-entity ent1)
                           (add-ecs-component (make-cmp-parent) ent1)
                           (add-ecs-component (make-cmp-independent) ent1)
                           ;; not registered to sys1
                           (add-ecs-component (make-cmp-parent) ent2)
                           (add-ecs-component (make-cmp-independent) ent2)
                           ;; not registered to sys1
                           (add-ecs-entity ent3)
                           (add-ecs-component (make-cmp-parent) ent3)
                           ;; execute
                           (ecs-main)
                           *test-counter*)
                         1)))
    (locally
        (declaim #+sbcl (sb-ext:muffle-conditions sb-ext:compiler-note))
      (prove-in-both (is-error (add-ecs-component (make-sample-entity)
                                                  (make-sample-entity))
                               'type-error))
      (prove-in-both (is-error (add-ecs-component (make-cmp-parent)
                                                  (make-cmp-parent))
                               'type-error))
      (with-modify-env
        (prove-in-both (is-error (let ((entity (make-sample-entity))
                                       (component (make-cmp-parent)))
                                   (add-ecs-component component entity)
                                   (add-ecs-component component entity))
                                 'simple-error))))))

(subtest
    "Test entity funcs"
  (subtest
      "Test get-ecs-component"
    (with-modify-env
      (prove-in-both (ok (typep (let ((entity (make-sample-entity)))
                                  (add-ecs-component (make-cmp-child) entity)
                                  (add-ecs-component (make-cmp-independent) entity)
                                  (get-ecs-component 'cmp-independent entity))
                                'cmp-independent)))
      (prove-in-both (ok (typep (let ((entity (make-sample-entity)))
                                  (add-ecs-component (make-cmp-child) entity)
                                  (add-ecs-component (make-cmp-independent) entity)
                                  (get-ecs-component 'cmp-parent entity))
                                'cmp-parent)))
      (prove-in-both (ok (null (let ((entity (make-sample-entity)))
                                 (add-ecs-component (make-cmp-parent) entity)
                                 (add-ecs-component (make-cmp-independent) entity)
                                 (get-ecs-component 'cmp-child entity)))))))
  (subtest
      "Test with-ecs-components"
    (with-modify-env
      (prove-in-both (is (let ((entity (make-sample-entity))
                               (counter 0))
                           (add-ecs-component (make-cmp-child) entity)
                           (add-ecs-component (make-cmp-independent) entity)
                           (with-ecs-components (cmp-parent cmp-independent) entity
                             ;; Note: In the current implementation, 'typep' for Parenscript
                             ;;       cannot judge the type correctly
                             (when cmp-parent ; (typep cmp-parent 'cmp-parent)
                               (incf counter))
                             (when cmp-independent ; (typep cmp-independent 'cmp-independent)
                               (incf counter)))
                           counter)
                         2))
      (prove-in-both (is-error (let ((entity (make-sample-entity)))
                                 (add-ecs-component (make-cmp-parent) entity)
                                 (add-ecs-component (make-cmp-independent) entity)
                                 (with-ecs-components (cmp-child cmp-independent) entity
                                   (+ 1 2)))
                               'simple-error)))
    (pass "TODO"))
  (subtest
      "Test add-ecs-entity"
    (with-modify-env
      (is-list.ps+ (add-sample-entities-for-inherit
                    (lambda (parent child)
                      (list (eq (sample-entity-parent child) parent)
                            (eq (nth 0 (sample-entity-children parent)) child))))
                   (list t t)))
    (with-modify-env
      (prove-in-both (is (let ((ent1 (make-sample-entity))
                               (ent2 (make-sample-entity))
                               (ent3 (make-sample-entity)))
                           (register-ecs-system "sys1" (make-sys-test1))
                           ;; registered to sys1
                           (add-ecs-component (make-cmp-parent) ent1)
                           (add-ecs-component (make-cmp-independent) ent1)
                           (add-ecs-entity ent1)
                           ;; not registered to sys1
                           (add-ecs-component (make-cmp-parent) ent2)
                           (add-ecs-component (make-cmp-independent) ent2)
                           ;; not registered to sys1
                           (add-ecs-component (make-cmp-parent) ent3)
                           (add-ecs-entity ent3)
                           ;; execute
                           (ecs-main)
                           *test-counter*)
                         1)))
    ;; test if descendants are registered to systems
    (with-modify-env
      (prove-in-both (is (add-sample-entities-for-inherit
                          (lambda (parent child) 
                            ;; prepare
                            (register-ecs-system "sys1" (make-sys-test1))
                            (add-components-for-sys-test1 parent)
                            (add-components-for-sys-test1 child)
                            ;; execute (*test-counter* should become 2) 
                            (ecs-main)
                            ;; delete and execute (*test-counter* should become 2) 
                            (delete-ecs-entity parent)
                            (ecs-main)
                            ;; add parent again and execute (*test-counter* should become 4) 
                            (add-ecs-entity parent)
                            (ecs-main)
                            *test-counter*))
                         4)))
    (locally
        (declaim #+sbcl (sb-ext:muffle-conditions sb-ext:compiler-note))
      (with-modify-env
        (prove-in-both (is-error (add-sample-entities-for-inherit
                                  (lambda (parent child)
                                    (add-ecs-entity child)))
                                 'simple-error)))
      (prove-in-both (is-error (add-ecs-entity (make-not-entity))
                               'type-error))
      (prove-in-both (is-error (add-ecs-entity (make-sample-entity) (make-not-entity))
                               'type-error))
      (prove-in-both (is-error (add-ecs-entity (make-sample-entity) (make-sample-entity))
                               'simple-error))))
  (subtest
      "Test delete-ecs-entities"
    ;; test if descendants are deleted
    (with-modify-env
      (is-list.ps+ (add-sample-entities-for-inherit
                    (lambda (parent child)
                      (let ((grandchild (make-sample-entity)))
                        (add-ecs-entity grandchild child)
                        (delete-ecs-entity child)
                        (list (null (sample-entity-parent child))
                              (null (nth 0 (sample-entity-children parent)))
                              (not (null (find-the-entity parent)))
                              (find-the-entity child)
                              (find-the-entity grandchild)))))
                   (list t t t nil nil)))
    ;; test deleteing toplevel entity
    (with-modify-env
      (is-list.ps+ (add-sample-entities-for-inherit
                    (lambda (parent child)
                      (delete-ecs-entity parent)
                      (list (eq (sample-entity-parent child) parent)
                            (eq (nth 0 (sample-entity-children parent)) child)
                            (find-the-entity parent)
                            (find-the-entity child))))
                   (list t t nil nil)))
    ;; test if the entity and its descendants are deleted from system
    (with-modify-env
      (prove-in-both (is (add-sample-entities-for-inherit
                          (lambda (parent child)
                            (let ((grandchild (make-sample-entity)))
                              ;; prepare
                              (add-ecs-entity grandchild child)
                              (register-ecs-system "sys1" (make-sys-test1))
                              (add-components-for-sys-test1 parent)
                              (add-components-for-sys-test1 child)
                              (add-components-for-sys-test1 grandchild)
                              ;; execute (*test-counter* should become 3)
                              (ecs-main)
                              ;; delete and execute
                              (delete-ecs-entity child)
                              (ecs-main)
                              *test-counter*)))
                         4)))
    ;; test error
    (locally
        (declaim #+sbcl (sb-ext:muffle-conditions sb-ext:compiler-note))
      (with-modify-env
        (prove-in-both (is-error (add-sample-entities-for-inherit
                                  (lambda (parent child)
                                    (declare (ignore parent child))
                                    (let ((not-registered (make-sample-entity)))
                                      (delete-ecs-entity not-registered))))
                                 'simple-error)))
      (prove-in-both (is-error (delete-ecs-entity (make-not-entity))
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
    (with-modify-env
      (prove-in-both (ok (not (progn (add-sample-entities-for-loop)
                                     (find-a-entity
                                      (lambda (ent) (eq ent 5)))))))))
  (subtest
      "Test find-the-entity"
    (with-modify-env
      (prove-in-both (is (progn (add-sample-entities-for-loop)
                                (let ((target
                                       (find-a-entity (lambda (ent)
                                                        (and (sample-entity-p ent)
                                                             (= (sample-entity-a ent) 3))))))
                                  (sample-entity-a (find-the-entity target))))
                         3)))
    (with-modify-env
      (prove-in-both (ok (not (progn (add-sample-entities-for-loop)
                                     (find-the-entity (make-sample-entity)))))))))

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
