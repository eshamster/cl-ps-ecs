(in-package :cl-user)
(defpackage cl-ps-ecs.utils
  (:use :cl
        :parenscript
        :ps-experiment)
  (:export :includes-all-component-types))
(in-package :cl-ps-ecs.utils)

;; ---- independent ---- ;;

(defun.ps+ includes-all-component-types (target-component-types components)
  (every (lambda (type)
           (some (lambda (comp)
                   (typep comp type))
                 components))
         target-component-types))

