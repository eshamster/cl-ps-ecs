(defpackage cl-ps-ecs/t/ecs
  (:use :cl
        :cl-ps-ecs
        :parenscript
        :ps-experiment
        :rove
        :ps-experiment/t/test-utils)
  (:import-from :cl-ps-ecs/ecs
                :clean-ecs-env))
(in-package :cl-ps-ecs/t/ecs)

(declaim #+sbcl (sb-ext:muffle-conditions sb-ext:compiler-note))

;; TOOD: push and restore the ps environment

;; ---- Definitions for test ---- ;;

;; - values and structs - ;;

(defstruct.ps+ (cmp-parent (:include ecs-component)))
(defstruct.ps+ (cmp-child (:include cmp-parent)))
(defstruct.ps+ (cmp-grand-child (:include cmp-child)))
(defstruct.ps+ (cmp-independent (:include ecs-component)) a)

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

(defun.ps+ count-components (entity)
  (check-type entity ecs-entity)
  (length (cl-ps-ecs/ecs::ecs-entity-components entity)))

;; ---- Start test ---- ;;

(deftest.ps+ component-functions
  (testing "Test add-ecs-component"
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
        (ok (= *test-counter* 1))))
    (testing "error case"
      (ok (signals (add-ecs-component (make-sample-entity)
                                      (make-sample-entity))
                   'type-error))
      (ok (signals (add-ecs-component (make-cmp-parent)
                                      (make-cmp-parent))
                   'type-error))
      (with-modify-env
        (ok (signals (let ((entity (make-sample-entity))
                           (component (make-cmp-parent)))
                       (add-ecs-component component entity)
                       (add-ecs-component component entity))
                     'simple-error)))))
  (testing "find-a-component"
    (with-modify-env
      (let ((ent (make-sample-entity))
            (cmp0 (make-cmp-independent :a 0))
            (cmp1 (make-cmp-independent :a 1))
            (cmp2 (make-cmp-independent :a 2))
            (cmp3 (make-cmp-independent :a 3)))
        (add-ecs-component cmp0 ent)
        (add-ecs-component cmp1 ent cmp0)
        (add-ecs-component cmp2 ent cmp0)
        (add-ecs-component cmp3 ent cmp2)
        (flet ((is-found (search-value top-cmp expected)
                 (let ((found (find-a-component
                               (lambda (cmp) (= (cmp-independent-a cmp) search-value))
                               top-cmp)))
                   ;; Note: In JavaScript, 0 is interpreted as false in if clause.
                   (if (not (null expected))
                       (ok (= (cmp-independent-a found)
                              expected))
                       (ok (not found))))))
          (dotimes (i 4)
            (is-found i cmp0 i))
          (is-found 99 cmp0 nil)
          ;; search only it and its descendant
          (is-found 0 cmp1 nil)))
      ;; check type-error
      (ok (signals (find-a-component (lambda (cmp) cmp) "not a component")
                   'type-error))))
  (testing "delete-ecs-component"
    (with-modify-env
      (let ((ent (make-sample-entity))
            (target-cmp (make-cmp-parent)))
        (register-ecs-system "sys1" (make-sys-test1))
        ;; registered to sys1
        (add-ecs-entity ent)
        (add-ecs-component-list
         ent
         target-cmp
         (make-cmp-independent))
        (delete-ecs-component target-cmp ent)
        (ok (get-ecs-component 'cmp-independent ent))
        (ng (get-ecs-component 'cmp-parent ent))
        (ok (signals (delete-ecs-component target-cmp ent)
                     'simple-error)))))
  (testing "recursive deletion"
    (with-modify-env
      (let ((ent (make-sample-entity))
            (parent-cmp (make-cmp-parent))
            (child-cmp (make-cmp-independent))
            (other-cmp (make-cmp-independent)))
        (add-ecs-component-list ent parent-cmp other-cmp)
        (add-ecs-component child-cmp ent parent-cmp)
        (ok (= (count-components ent) 3))
        (delete-ecs-component parent-cmp ent)
        (ok (= (count-components ent) 1)))))
  (testing "Test delete-component-hook"
    (with-modify-env
      (let* ((counter 0)
             (ent (make-sample-entity))
             (target-cmp (make-cmp-parent))
             (child-cmp (make-cmp-child))
             (other-cmp1 (make-cmp-independent))
             (other-cmp2 (make-cmp-independent))
             (test-callback (lambda (component)
                              (incf counter
                                    (cond
                                      ((eq component target-cmp) 100)
                                      ((eq component child-cmp) 10)
                                      (t 1))))))
        (add-ecs-component-list
         ent target-cmp other-cmp1 other-cmp2)
        (add-ecs-component child-cmp ent target-cmp)
        (add-ecs-entity ent)
        (ok (= counter 0))
        (add-delete-component-hook test-callback)
        (delete-ecs-component target-cmp ent)
        (ok (= counter 110))
        (delete-ecs-component other-cmp1 ent)
        (ok (= counter 111))
        (delete-delete-component-hook test-callback)
        ;; The hook is no longer invoked
        (delete-ecs-component other-cmp2 ent)
        (ok (= counter 111))
        ;; Error because of duplicated deletion
        (ok (signals (delete-delete-component-hook test-callback)
                     'simple-error)))))
  (testing "delete-ecs-component-type"
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
        (ok (= *test-counter* 2))
        (ecs-main)
        (ok (= *test-counter* 3))))
    (testing "recursive deletion"
      (with-modify-env
        (let ((ent (make-sample-entity))
              (parent-cmp (make-cmp-parent))
              (child-cmp (make-cmp-independent))
              (other-cmp (make-cmp-independent)))
          (add-ecs-component-list ent parent-cmp other-cmp)
          (add-ecs-component child-cmp ent parent-cmp)
          (ecs-main)
          (ok (= (count-components ent) 3))
          (delete-ecs-component-type 'cmp-parent ent)
          (ok (= (count-components ent) 1)))))
    (testing "type error"
      (ok (signals (delete-ecs-component-type 'cmp-parent 12)
                   'type-error)))))

(deftest.ps+ |Check the reference change of *entity-list*|
  ;; Note: In delete-ecs-entity, it replaces *entity-list* by a new list.
  ;; So if we simply refer cl-ps-ecs/ecs::*entity-list* (in the JavaScript,
  ;; it becomes clPsEcs_ecs._internal.ENTITYLIST), such change cannot be
  ;; reflected to the referrer in JavaScript side.
  ;; This test checks if we can avoid the issue.
  (with-modify-env
    (let ((entity (make-ecs-entity)))
      (add-ecs-entity entity)
      (ok (= (count-entity-list) 1))
      (delete-ecs-entity entity)
      (ok (= (count-entity-list) 0)))))

(deftest.ps+ entity-functions
  (testing "Test ecs-entity-id"
    (with-modify-env
      (let* ((ent1 (make-sample-entity))
             (ent2 (make-sample-entity))
             (id1 (ecs-entity-id ent1))
             (id2 (ecs-entity-id ent2)))
        (ok (numberp id1))
        (ok (numberp id2))
        (ng (= id1 id2)))))
  (testing "Test get-ecs-component"
    (with-modify-env
      (let ((entity (make-sample-entity)))
        (add-ecs-component (make-cmp-child) entity)
        (add-ecs-component (make-cmp-independent) entity)
        (ok (typep (get-ecs-component 'cmp-independent entity)
                   'cmp-independent))
        (ok (typep (get-ecs-component 'cmp-parent entity)
                   'cmp-parent))
        (ok (null (get-ecs-component 'cmp-grand-child entity))))))
  (testing "Test with-ecs-components"
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
        (ok (signals (with-ecs-components (cmp-child cmp-independent) entity
                       (print cmp-child)
                       (print cmp-independent))
                     'simple-error)))))
  (testing "Test add-ecs-entity"
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
        (ok (= *test-counter* 1))))
    (testing "with-ecs-entity-parent"
      (with-modify-env
        (let ((parent (make-ecs-entity))
              (child (make-ecs-entity)))
          (with-ecs-entity-parent (parent)
            (ok (find-the-entity parent))
            (add-ecs-entity child)
            (ok (eq (ecs-entity-parent child) parent))
            (delete-ecs-entity parent)
            (ok (not (find-the-entity child)))))
        (let ((entity (make-ecs-entity)))
          (add-ecs-entity entity)
          (ok (not (ecs-entity-parent entity))))))
    (testing "test if descendants are registered to systems"
      (with-modify-env
        (add-sample-entities-for-inherit
         (lambda (parent child) 
           ;; prepare
           (register-ecs-system "sys1" (make-sys-test1))
           (add-components-for-sys-test1 parent)
           (add-components-for-sys-test1 child)
           ;; execute
           (ecs-main)
           (ok (= *test-counter* 2))
           ;; delete parent and execute
           (delete-ecs-entity parent)
           (ecs-main)
           (ok (= *test-counter* 2))
           ;; add parent again and execute
           (add-ecs-entity parent)
           (ecs-main)
           (ok (= *test-counter* 4))))))
    (testing "error case"
      (with-modify-env
        (add-sample-entities-for-inherit
         (lambda (parent child)
           (declare (ignore parent))
           (ok (signals (add-ecs-entity child) 'simple-error)))))
      (ok (signals (add-ecs-entity (make-not-entity))
                   'type-error))
      (ok (signals (add-ecs-entity (make-sample-entity) (make-not-entity))
                   'type-error))
      (ok (signals (add-ecs-entity (make-sample-entity) (make-sample-entity))
                   'simple-error))))
  (testing "Test add-ecs-entity-to-buffer"
    (with-modify-env
      (let ((parent (make-sample-entity)))
        (add-ecs-entity (make-sample-entity))
        (add-ecs-entity-to-buffer parent)
        (add-ecs-entity-to-buffer (make-sample-entity) parent)
        (ok (= (count-entity-list) 1))
        (ecs-main)
        (ok (= (count-entity-list) 3)))))
  (testing "Test delete-ecs-entities"
    (testing "test if descendants are deleted"
      (with-modify-env
        (add-sample-entities-for-inherit
         (lambda (parent child)
           (let ((grandchild (make-sample-entity)))
             (add-ecs-entity grandchild child)
             (delete-ecs-entity child)
             (ok (null (sample-entity-parent child)))
             (ok (null (nth 0 (sample-entity-children parent))))
             (ok (find-the-entity parent))
             (ng (find-the-entity child))
             (ng (find-the-entity grandchild)))))))
    (testing "test deleteing toplevel entity"
      (with-modify-env
        (add-sample-entities-for-inherit
         (lambda (parent child)
           (delete-ecs-entity parent)
           (ok (eq (sample-entity-parent child) parent))
           (ok (eq (nth 0 (sample-entity-children parent)) child))
           (ok (not (find-the-entity parent)))
           (ok (not (find-the-entity child)))))))
    (testing "test if the entity and its descendants are deleted from system"
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
             (ok (= *test-counter* 3))
             ;; delete and execute
             (delete-ecs-entity child)
             (ecs-main)
             (ok (= *test-counter* 4)))))))
    (testing "error case"
      (with-modify-env
        (add-sample-entities-for-inherit
         (lambda (parent child)
           (declare (ignore parent child))
           (let ((not-registered (make-sample-entity)))
             (ok (signals (delete-ecs-entity not-registered) 'simple-error)))))) 
      (ok (signals (delete-ecs-entity (make-not-entity))
                   'type-error))))
  (testing "Test do-ecs-entities"
    (with-modify-env
      (let ((sum 0))
        (add-sample-entities-for-loop)
        (do-ecs-entities ent
          (incf sum (sample-entity-a ent)))
        (ok (= sum 10)))))
  (testing "Test find-a-entity"
    (with-modify-env
      (add-sample-entities-for-loop)
      (ok (= (sample-entity-a
              (find-a-entity (lambda (ent)
                               (and (sample-entity-p ent)
                                    (= (sample-entity-a ent) 3)))))
             3))
      (ng (progn (add-sample-entities-for-loop)
                 (find-a-entity
                  (lambda (ent) (eq ent 5)))))))
  (testing "Test find-the-entity"
    (with-modify-env
      (add-sample-entities-for-loop)
      (let ((target
             (find-a-entity (lambda (ent)
                              (and (sample-entity-p ent)
                                   (= (sample-entity-a ent) 3))))))
        (ok (= (sample-entity-a (find-the-entity target))
               3)))
      (ng (find-the-entity (make-sample-entity))))))

(deftest.ps+ tag-functions
  (testing "Basic functions"
    (with-modify-env
      (let ((entity (make-sample-entity)))
        (ng (has-entity-tag entity "Tag1"))
        (add-entity-tag entity "Tag1")
        (add-entity-tag entity "Tag2" "Tag3")
        (ok (has-entity-tag entity "Tag1"))
        (ok (has-entity-tag entity "Tag2"))
        (ok (has-entity-tag entity "Tag3"))
        (delete-entity-tag entity "Tag1")
        (ng (has-entity-tag entity "Tag1")))))
  (testing "check functions"
    (let ((entity (make-sample-entity)))
      (add-entity-tag entity "Tag1")
      (add-entity-tag entity "Tag2")
      (add-entity-tag entity "Tag3")
      (ok (check-entity-tags entity "Tag2")) 
      (ok (check-entity-tags entity "Tag1" "Tag3"))
      (ok (signals (check-entity-tags entity "Not-exist")
                   'simple-error))
      (ok (signals (check-entity-tags entity "Tag1" "Not-exist")
                   'simple-error))))
  (testing "Auxiliary functions and macros"
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
        (ng (find-a-entity-by-tag "Not-found"))
        (let ((sum 0))
          (do-tagged-ecs-entities (entity "Tag1")
            (incf sum))
          (ok (= sum 2)))))))

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

(deftest.ps+ system-functions
  (testing "Test register-ecs-system"
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
        (ok (= *test-counter* 2))))
    (ok (signals (register-ecs-system (make-cmp-parent)
                                      (make-sample-entity))
                 'type-error))
    (ok (signals (register-ecs-system (make-sys-test1)
                                      (make-cmp-parent))
                 'type-error)))
  (testing "Test the order of the registeration."
    (with-modify-env
      (let ((entity (make-sample-entity)))
        (add-ecs-component (make-cmp-independent) entity)
        (add-ecs-entity entity)
        ;; counter should be 1
        (register-ecs-system "+" (make-sys-order1))
        ;; counter should be 2
        (register-ecs-system "*" (make-sys-order2))
        (ecs-main)
        (ok (= *test-counter* 2))))
    (with-modify-env
      (let ((entity (make-sample-entity)))
        (add-ecs-component (make-cmp-independent) entity)
        (add-ecs-entity entity)
        ;; counter should be 0
        (register-ecs-system "*" (make-sys-order2))
        ;; counter should be 1
        (register-ecs-system "+" (make-sys-order1))
        (ecs-main)
        (ok (= *test-counter* 1)))))
  (testing "Test process-all"
    (with-modify-env
      (let ((system (make-sys-test1
                     :process-all
                     (lambda (system)
                       (check-type system sys-test1)
                       (incf *test-counter*)))))
        (register-ecs-system "test-process-all" system)
        (ecs-main)
        (ok (= *test-counter* 1)))))
  (testing "Test hooks of ecs-system"
    (with-modify-env
      (let ((ent-target (make-sample-entity))
            (ent-not-target (make-sample-entity)))
        (register-ecs-system "test-hook" (make-sys-test-hook))
        (add-ecs-component (make-cmp-parent) ent-target)
        ;; *test-counter* must be +1 by add hook
        (add-ecs-entity ent-target)
        (ok (= *test-counter* 1))
        (add-ecs-entity ent-not-target)
        (ok (= *test-counter* 1))
        ;; *test-counter* must be +100 by delete hook
        (delete-ecs-entity ent-target)
        (ok (= *test-counter* 101))
        (delete-ecs-entity ent-not-target)
        (ok (= *test-counter* 101))))))

(deftest.ps+ for-do-ecs-components-of-entity
  (with-modify-env
    (let ((entity (make-sample-entity))
          (sum 0))
      (add-ecs-component (make-cmp-parent) entity)
      (add-ecs-component (make-cmp-independent) entity)
      (do-ecs-components-of-entity (component entity)
        (cond ((cmp-parent-p component) (incf sum 1))
              ((cmp-independent-p component) (incf sum 4))
              (t (incf sum 10000))))
      (ok (= sum 5))
      (add-ecs-component (make-cmp-child) entity)
      (do-ecs-components-of-entity (component entity
                                              :component-type 'cmp-parent)
        (cond ((cmp-parent-p component) (incf sum 1))
              ((cmp-independent-p component) (incf sum 4))
              (t (incf sum 10000))))
      (ok (= sum 7)))))
