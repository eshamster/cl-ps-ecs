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
                :with-prove-in-both
                :is-list.ps+))
(in-package :cl-ps-ecs-test.ecs)

(plan 7)

(declaim #+sbcl (sb-ext:muffle-conditions sb-ext:compiler-note))

;; TOOD: push and restore the ps environment

;; ---- Definitions for test ---- ;;

;; - values and structs - ;;

(defstruct.ps+ (cmp-parent (:include ecs-component)))
(defstruct.ps+ (cmp-child (:include cmp-parent)))
(defstruct.ps+ (cmp-grand-child (:include cmp-child)))
(defstruct.ps+ (cmp-independent (:include ecs-component)))

(defvar.ps+ *test-counter* 0)

(defstruct.ps+ (sys-test1 (:include ecs-system
                                    (target-component-types '(cmp-parent cmp-independent))
                                    (process (lambda (entity)
                                               (declare (ignore entity))
                                               (incf *test-counter*))))))
(defstruct.ps+ (sys-test-hook
                (:include ecs-system
                          (target-component-types '(cmp-parent))
                          (add-entity-hook (lambda (entity)
                                             (when (ecs-entity-p entity)
                                               (incf *test-counter* 1))))
                          (delete-entity-hook (lambda (entity)
                                                (when (ecs-entity-p entity)
                                                  (incf *test-counter* 100)))))))

(defun.ps+ add-components-for-sys-test1 (entity)
  (add-ecs-component (make-cmp-parent) entity)
  (add-ecs-component (make-cmp-independent) entity))

(defstruct.ps+ (sample-entity (:include ecs-entity)) a)
(defstruct.ps+ not-entity a b)

;; - funcs and macros - ;;

(defmacro.ps+ with-modify-env (&body body)
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

(defun.ps+ count-entity-list ()
  (let ((count 0))
    (do-ecs-entities entity
      (incf count))
    count))

;; ---- Start test ---- ;;

(subtest
    "Test component funcs"
  (subtest
      "Test add-ecs-component" 
    (with-prove-in-both ()
      (with-modify-env
        (let ((ent1 (make-sample-entity))
              (ent2 (make-sample-entity))
              (ent3 (make-sample-entity)))
          (register-ecs-system "sys1" (make-sys-test1))
          ;; registered to sys1
          (add-ecs-entity ent1)
          (add-ecs-component-list
           ent1
           (make-cmp-parent)
           (make-cmp-independent))
          ;; not registered to sys1
          (add-ecs-component (make-cmp-parent) ent2)
          (add-ecs-component (make-cmp-independent) ent2)
          ;; not registered to sys1
          (add-ecs-entity ent3)
          (add-ecs-component (make-cmp-parent) ent3)
          ;; execute
          (ecs-main)
          (is *test-counter* 1))))
    (with-prove-in-both ()
      (is-error (add-ecs-component (make-sample-entity)
                                   (make-sample-entity))
                'type-error)
      (is-error (add-ecs-component (make-cmp-parent)
                                   (make-cmp-parent))
                'type-error)
      (with-modify-env
        (is-error (let ((entity (make-sample-entity))
                        (component (make-cmp-parent)))
                    (add-ecs-component component entity)
                    (add-ecs-component component entity))
                  'simple-error))))
  (subtest
      "Test delete-ecs-component-type"
    (with-prove-in-both ()
      (with-modify-env 
        (let ((ent1 (make-sample-entity))
              (ent2 (make-sample-entity)))
          (register-ecs-system "sys1" (make-sys-test1))
          ;; registered to sys1
          (add-ecs-entity ent1)
          (add-ecs-component-list
           ent1
           (make-cmp-parent)
           (make-cmp-independent))
          (add-ecs-entity ent2)
          (add-ecs-component-list
           ent2
           (make-cmp-parent)
           (make-cmp-independent))
          ;; execute (counter +2)
          (ecs-main)
          ;; execute (counter +1)
          (delete-ecs-component-type 'cmp-parent ent1)
          (ok (get-ecs-component 'cmp-independent ent1))
          (ok (not (get-ecs-component 'cmp-parent ent1)))
          (is *test-counter* 2)
          (ecs-main)
          (is *test-counter* 3))))
    (with-prove-in-both ()
      (is-error (delete-ecs-component-type 'cmp-parent 12)
                'type-error))))

(subtest
    "Test entity funcs"
  (subtest
      "Test ecs-entity-id"
    (with-prove-in-both ()
      (with-modify-env
        (let* ((ent1 (make-sample-entity))
               (ent2 (make-sample-entity))
               (id1 (ecs-entity-id ent1))
               (id2 (ecs-entity-id ent2)))
          (ok (numberp id1))
          (ok (numberp id2))
          (ok (not (= id1 id2)))))))
  (subtest
      "Test get-ecs-component"
    (with-prove-in-both ()
      (with-modify-env
        (let ((entity (make-sample-entity)))
          (add-ecs-component (make-cmp-child) entity)
          (add-ecs-component (make-cmp-independent) entity)
          (ok (typep (get-ecs-component 'cmp-independent entity)
                     'cmp-independent))
          (ok (typep (get-ecs-component 'cmp-parent entity)
                     'cmp-parent))
          (ok (null (get-ecs-component 'cmp-grand-child entity)))))))
  (subtest
      "Test with-ecs-components"
    (with-prove-in-both ()
      (with-modify-env
        (let ((entity (make-sample-entity)))
          (add-ecs-component (make-cmp-child) entity)
          (add-ecs-component (make-cmp-independent) entity)
          (with-ecs-components (cmp-parent (test cmp-independent)) entity
            ;; Note: In the current implementation, 'typep' for Parenscript
            ;;       cannot judge the type correctly
            (ok cmp-parent); (typep cmp-parent 'cmp-parent)
            (ok test) ; (typep test 'cmp-independent))
            )))
      (with-modify-env
        (let ((entity (make-sample-entity)))
          (add-ecs-component (make-cmp-parent) entity)
          (add-ecs-component (make-cmp-independent) entity)
          (is-error (with-ecs-components (cmp-child cmp-independent) entity
                      (print cmp-child)
                      (print cmp-independent))
                    'simple-error)))))
  (subtest
      "Test add-ecs-entity"
    (with-prove-in-both ()
      (with-modify-env
        (add-sample-entities-for-inherit
         (lambda (parent child)
           (ok (eq (sample-entity-parent child) parent))
           (ok (eq (nth 0 (sample-entity-children parent)) child))))) 
      (with-modify-env
        (let ((ent1 (make-sample-entity))
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
          (is *test-counter* 1)))
      ;; test if descendants are registered to systems
      (with-modify-env
        (add-sample-entities-for-inherit
         (lambda (parent child) 
           ;; prepare
           (register-ecs-system "sys1" (make-sys-test1))
           (add-components-for-sys-test1 parent)
           (add-components-for-sys-test1 child)
           ;; execute
           (ecs-main)
           (is *test-counter* 2)
           ;; delete parent and execute
           (delete-ecs-entity parent)
           (ecs-main)
           (is *test-counter* 2)
           ;; add parent again and execute
           (add-ecs-entity parent)
           (ecs-main)
           (is *test-counter* 4))))
      (with-modify-env
        (add-sample-entities-for-inherit
         (lambda (parent child)
           (declare (ignore parent))
           (is-error (add-ecs-entity child) simple-error))))
      (is-error (add-ecs-entity (make-not-entity))
                'type-error)
      (is-error (add-ecs-entity (make-sample-entity) (make-not-entity))
                'type-error)
      (is-error (add-ecs-entity (make-sample-entity) (make-sample-entity))
                'simple-error)))
  (subtest
      "Test add-ecs-entity-to-buffer"
    (with-prove-in-both ()
      (with-modify-env
        (let ((parent (make-sample-entity)))
          (add-ecs-entity (make-sample-entity))
          (add-ecs-entity-to-buffer parent)
          (add-ecs-entity-to-buffer (make-sample-entity) parent)
          (is (count-entity-list) 1)
          (ecs-main)
          (is (count-entity-list) 3)))))
  (subtest
      "Test delete-ecs-entities"
    (with-prove-in-both ()
      ;; test if descendants are deleted
      (with-modify-env
        (add-sample-entities-for-inherit
         (lambda (parent child)
           (let ((grandchild (make-sample-entity)))
             (add-ecs-entity grandchild child)
             (delete-ecs-entity child)
             (ok (null (sample-entity-parent child)))
             (ok (null (nth 0 (sample-entity-children parent))))
             (ok (find-the-entity parent))
             (ok (not (find-the-entity child)))
             (ok (not (find-the-entity grandchild)))))))
      ;; test deleteing toplevel entity
      (with-modify-env
        (add-sample-entities-for-inherit
         (lambda (parent child)
           (delete-ecs-entity parent)
           (ok (eq (sample-entity-parent child) parent))
           (ok (eq (nth 0 (sample-entity-children parent)) child))
           (ok (not (find-the-entity parent)))
           (ok (not (find-the-entity child))))))
      ;; test if the entity and its descendants are deleted from system
      (with-modify-env
        (add-sample-entities-for-inherit
         (lambda (parent child)
           (let ((grandchild (make-sample-entity)))
             ;; prepare
             (add-ecs-entity grandchild child)
             (register-ecs-system "sys1" (make-sys-test1))
             (add-components-for-sys-test1 parent)
             (add-components-for-sys-test1 child)
             (add-components-for-sys-test1 grandchild)
             ;; execute
             (ecs-main)
             (is *test-counter* 3)
             ;; delete and execute
             (delete-ecs-entity child)
             (ecs-main)
             (is *test-counter* 4))))))
    ;; test error
    (with-prove-in-both ()
      (with-modify-env
        (add-sample-entities-for-inherit
         (lambda (parent child)
           (declare (ignore parent child))
           (let ((not-registered (make-sample-entity)))
             (is-error (delete-ecs-entity not-registered) 'simple-error))))) 
      (is-error (delete-ecs-entity (make-not-entity))
                'type-error)))
  (subtest
      "Test do-ecs-entities"
    (with-prove-in-both ()
      (with-modify-env
        (let ((sum 0))
          (add-sample-entities-for-loop)
          (do-ecs-entities ent
            (incf sum (sample-entity-a ent)))
          (is sum 10)))))
  (subtest
      "Test find-a-entity"
    (with-prove-in-both ()
      (with-modify-env
        (add-sample-entities-for-loop)
        (is (sample-entity-a
             (find-a-entity (lambda (ent)
                              (and (sample-entity-p ent)
                                   (= (sample-entity-a ent) 3)))))
            3)
        (ok (not (progn (add-sample-entities-for-loop)
                        (find-a-entity
                         (lambda (ent) (eq ent 5)))))))))
  (subtest
      "Test find-the-entity"
    (with-prove-in-both ()
      (with-modify-env
        (add-sample-entities-for-loop)
        (let ((target
               (find-a-entity (lambda (ent)
                                (and (sample-entity-p ent)
                                     (= (sample-entity-a ent) 3))))))
          (is (sample-entity-a (find-the-entity target))
              3))
        (ok (not (find-the-entity (make-sample-entity))))))))

(subtest
    "Test functions about tag"
  (subtest
      "Basic functions"
    (with-prove-in-both ()
      (with-modify-env
        (let ((entity (make-sample-entity)))
          (ok (not (has-entity-tag entity "Tag1")))
          (add-entity-tag entity "Tag1")
          (add-entity-tag entity "Tag2" "Tag3")
          (ok (has-entity-tag entity "Tag1"))
          (ok (has-entity-tag entity "Tag2"))
          (ok (has-entity-tag entity "Tag3"))
          (delete-entity-tag entity "Tag1")
          (ok (not (has-entity-tag entity "Tag1")))))))
  (subtest
      "check functions"
    (with-prove-in-both ()
      (let ((entity (make-sample-entity)))
        (add-entity-tag entity "Tag1")
        (add-entity-tag entity "Tag2")
        (add-entity-tag entity "Tag3")
        (ok (check-entity-tags entity "Tag2")) 
        (ok (check-entity-tags entity "Tag1" "Tag3"))
        (is-error (check-entity-tags entity "Not-exist")
                  'simple-error)
        (is-error (check-entity-tags entity "Tag1" "Not-exist")
                  'simple-error))))
  (subtest
      "Auxiliary functions and macros"
    (with-prove-in-both ()
      (with-modify-env
        (let ((entity-list (list (make-sample-entity)
                                 (make-sample-entity)
                                 (make-sample-entity))))
          (dolist (entity entity-list)
            (add-entity-tag entity "Tag1")
            (add-entity-tag entity "Tag2")
            (add-ecs-entity entity))
          (delete-entity-tag (nth 2 entity-list) "Tag1")
          (let ((found (find-a-entity-by-tag "Tag1")))
            (check-type found ecs-entity))
          (ok (not (find-a-entity-by-tag "Not-found")))
          (let ((sum 0))
            (do-tagged-ecs-entities (entity "Tag1")
              (incf sum))
            (is sum 2)))))))

(defstruct.ps+ (sys-order1 (:include ecs-system
                                     (target-component-types '(cmp-independent))
                                     (process (lambda (entity)
                                                (declare (ignore entity))
                                                (incf *test-counter*))))))

(defstruct.ps+ (sys-order2 (:include ecs-system
                                     (target-component-types '(cmp-independent))
                                     (process (lambda (entity)
                                                (declare (ignore entity))
                                                (setf *test-counter*
                                                      (* *test-counter* 2)))))))

(subtest
    "Test system funcs"
  (subtest
      "Test register-ecs-system"
    (with-prove-in-both ()
      (with-modify-env
        (let ((ent1 (make-sample-entity))
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
          (is *test-counter* 2)))
      (is-error (register-ecs-system (make-cmp-parent)
                                     (make-sample-entity))
                'type-error)
      (is-error (register-ecs-system (make-sys-test1)
                                     (make-cmp-parent))
                'type-error))
    (subtest
        "Test the order of the registeration."
      (with-prove-in-both ()
        (with-modify-env
          (let ((entity (make-sample-entity)))
            (add-ecs-component (make-cmp-independent) entity)
            (add-ecs-entity entity)
            ;; counter should be 1
            (register-ecs-system "+" (make-sys-order1))
            ;; counter should be 2
            (register-ecs-system "*" (make-sys-order2))
            (ecs-main)
            (is *test-counter* 2)))
        (with-modify-env
          (let ((entity (make-sample-entity)))
            (add-ecs-component (make-cmp-independent) entity)
            (add-ecs-entity entity)
            ;; counter should be 0
            (register-ecs-system "*" (make-sys-order2))
            ;; counter should be 1
            (register-ecs-system "+" (make-sys-order1))
            (ecs-main)
            (is *test-counter* 1))))))
  (subtest
      "Test process-all"
    (with-prove-in-both ()
      (with-modify-env
        (let ((system (make-sys-test1
                       :process-all
                       (lambda (system)
                         (check-type system sys-test1)
                         (incf *test-counter*)))))
          (register-ecs-system "test-process-all" system)
          (ecs-main)
          (is *test-counter* 1)))))
  (subtest
      "Test hooks of ecs-system"
    (with-prove-in-both ()
      (with-modify-env
        (let ((ent-target (make-sample-entity))
              (ent-not-target (make-sample-entity)))
          (register-ecs-system "test-hook" (make-sys-test-hook))
          (add-ecs-component (make-cmp-parent) ent-target)
          ;; *test-counter* must be +1 by add hook
          (add-ecs-entity ent-target)
          (is *test-counter* 1)
          (add-ecs-entity ent-not-target)
          (is *test-counter* 1)
          ;; *test-counter* must be +100 by delete hook
          (delete-ecs-entity ent-target)
          (is *test-counter* 101)
          (delete-ecs-entity ent-not-target)
          (is *test-counter* 101))))))

(subtest
    "Test register-next-frame-func"
  (with-prove-in-both ()
    (with-modify-env
      (let ((counter 0))
        ;; Check if 2 functions are executed in order of registration
        (register-next-frame-func #'(lambda () (incf counter)))
        (register-next-frame-func #'(lambda () (setf counter (* 2 counter))))
        (is counter 0)
        (incf counter 10)
        (is counter 10)
        (ecs-main)
        (is counter 22)
        ;; Check if registered functions are cleared after execution
        (ecs-main)
        (is counter 22)))))

(subtest
    "Test register-func-with-pred"
  (with-prove-in-both ()
    (with-modify-env
      (let ((counter 0))
        (flet ((make-adder (n)
                 (lambda () (incf counter n)))
               (make-pred (border)
                 (lambda () (>= counter border))))
          ;; func1
          (register-func-with-pred (make-adder 10) (make-pred -1))
          ;; func2
          (register-func-with-pred (make-adder 100) (make-pred 10))
          ;; func3
          (register-func-with-pred (make-adder 1000) (make-pred 999999)
                                   :timeout-frame 3)
          (is counter 0)
          ;; only func1 should be invoked (and removed)
          (ecs-main)
          (is counter 10)
          ;; only func2 should be invokded
          (ecs-main)
          (is counter 110)
          ;; should be timeout error because of func3
          (is-error (ecs-main) 'simple-error))))))

(subtest
    "Test do-ecs-components-of-entity"
  (with-prove-in-both ()
    (with-modify-env
      (let ((entity (make-sample-entity))
            (sum 0))
        (add-ecs-component (make-cmp-parent) entity)
        (add-ecs-component (make-cmp-independent) entity)
        (do-ecs-components-of-entity (component entity)
          (cond ((cmp-parent-p component) (incf sum 1))
                ((cmp-independent-p component) (incf sum 4))
                (t (incf sum 10000))))
        (is sum 5)
        (add-ecs-component (make-cmp-child) entity)
        (do-ecs-components-of-entity (component entity
                                                :component-type 'cmp-parent)
          (cond ((cmp-parent-p component) (incf sum 1))
                ((cmp-independent-p component) (incf sum 4))
                (t (incf sum 10000))))
        (is sum 7)))))

(finalize)
