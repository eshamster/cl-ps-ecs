(in-package :cl-user)
(defpackage cl-ps-ecs.ecs
  (:use :cl
        :parenscript
        :ps-experiment)
  (:export :includes-all-component-types))
(in-package :cl-ps-ecs.ecs)

;; ;; ---- component ---- ;;
;; (defstruct.ps+ ecs-component)

;; ;; ---- entity ---- ;;
;; (defvar.ps+ *entity-id-counter* 0)

;; (defstruct.ps+ ecs-entity
;;   (id (incf *entity-id-counter*))
;;   (components nil)
;;   parent
;;   (children '()))

;; (defvar.ps *entity-tree-head* (make-ecs-entity))

;; ;; ---- system ---- ;;
;; (defvar.ps+ *ecs-system-list* nil)

;; (defstruct.ps+ ecs-system
;;   (enable t)
;;   (target-entities '())
;;   (target-component-types '())
;;   (process (lambda ())))

;; (defun.ps+ ecs-main ()
;;   (dolist (system *ecs-system-list*)
;;     (when (ecs-system-enable system)
;;       (funcall (ecs-system-process system)))))

;; ---- Cross cutting ---- ;;

;; component

(defun.ps+ includes-all-component-types (target-component-types components)
  (every (lambda (type)
           (some (lambda (comp)
                   (typep comp type))
                 components))
         target-component-types))

;; entity component system

(defun.ps+ add-ecs-component (entity component)
  )

(defun.ps+ remove-ecs-component (entity component))

