(in-package :cl-user)
(defpackage cl-ps-ecs.utils
  (:use :cl
        :parenscript
        :ps-experiment)
  (:export :includes-all-component-types
           :push-to-ecs-hooks
           :call-ecs-hooks
           :delete-from-ecs-hooks
           :clean-ecs-hooks))
(in-package :cl-ps-ecs.utils)

(defun.ps+ includes-all-component-types (target-component-types components)
  (every (lambda (type)
           (some (lambda (comp)
                   (typep comp type))
                 components))
         target-component-types))

;; -- hook -- ;;

(defun.ps+ call-ecs-hooks (object type is-added hooks)
  (unless (typep object type)
    (error 'type-error :expected-type type :datum object))
  (dolist (callback hooks)
    (funcall callback object is-added)))

(defmacro.ps+ push-to-ecs-hooks (callback hooks)
  "callback is the function that take 2 arguments. The first is an object, and the second is a boolean (t: at adding time; nil: at deleting time)"
  `(progn (when (find ,callback ,hooks)
            (error "The callback have already been added"))
          (push ,callback ,hooks)))

(defmacro.ps+ delete-from-ecs-hooks (callback hooks)
  `(progn (unless (find ,callback ,hooks)
            (error "The call back have not been added"))
          (setf ,hooks (remove ,callback ,hooks))))

(defmacro.ps+ clean-ecs-hooks (hooks)
  `(setf ,hooks '()))
