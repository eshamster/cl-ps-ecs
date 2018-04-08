(defpackage cl-ps-ecs/t/utils
  (:use :cl
        :cl-ps-ecs/utils
        :parenscript
        :ps-experiment
        :rove
        :ps-experiment/t/test-utils))
(in-package :cl-ps-ecs/t/utils)


(defstruct.ps+ test1 a b)
(defstruct.ps+ (test2 (:include test1)) c)
(defstruct.ps+ test3 a b)

(deftest.ps+ for-includes-all-component-types
  (ok (includes-all-component-types
       '(test1 test3)
       (list (make-test2) (make-test3))))
  (ok (not (includes-all-component-types
            '(test1 test3)
            (list (make-test3)))))
  (ok (not (includes-all-component-types
            '(test3)
            (list (make-test1) (make-test2)))))
  (ok (includes-all-component-types
       '()
       (list (make-test1) (make-test2)))))

(defstruct.ps+ test-st-hook1 a)
(defstruct.ps+ test-st-hook2 b)

(defvar.ps+ *test-var-hook* 0)

(defun.ps+ test-fn-hook1 (object is-added)
  (declare (ignore object))
  (if is-added
      (incf *test-var-hook*)
      (decf *test-var-hook*)))

(defun.ps+ test-fn-hook2 (object is-added)
  (declare (ignore object))
  (if is-added
      (incf *test-var-hook* 10)
      (decf *test-var-hook* 10)))

(defun.ps+ init-test-hooks ()
  (let ((hooks '()))
    (push-to-ecs-hooks #'test-fn-hook1 hooks)
    (push-to-ecs-hooks #'test-fn-hook2 hooks)))

(deftest.ps+ functions-for-hook
  (ok (= (let ((hooks (init-test-hooks)))
           (call-ecs-hooks (make-test-st-hook1) 'test-st-hook1 t hooks)
           *test-var-hook*)
         11))
  (setf *test-var-hook* 0)
  (ok (= (let ((hooks (init-test-hooks)))
           (call-ecs-hooks (make-test-st-hook1) 'test-st-hook1 nil hooks)
           *test-var-hook*)
         -11))
  (setf *test-var-hook* 0)
  (ok (= (let ((hooks (init-test-hooks)))
           (call-ecs-hooks (make-test-st-hook1) 'test-st-hook1 t hooks)
           (delete-from-ecs-hooks #'test-fn-hook2 hooks)
           (call-ecs-hooks (make-test-st-hook1) 'test-st-hook1 t hooks)
           *test-var-hook*)
         12))
  (setf *test-var-hook* 0)
  (ok (signals (call-ecs-hooks (make-test-st-hook1) 'test-st-hook2 t '())
               'type-error)))
