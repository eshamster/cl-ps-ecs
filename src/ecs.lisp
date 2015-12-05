(in-package :cl-user)
(defpackage cl-ps-ecs.ecs
  (:use :cl
        :parenscript
        :ps-experiment)
  (:export :includes-all-component-types
           :ecs-entity
           :add-ecs-entity
           :clean-ecs-entities
           :process-all-entities
           :def-ecs-system
           :ecs-main)
  (:import-from :alexandria
                :symbolicate))
(in-package :cl-ps-ecs.ecs)

;; ---- component ---- ;;
;; This has dummy data to avoid error in cl-js:js-run with Clozure CL
(defstruct.ps+ ecs-component
    dummy)

;; ---- entity ---- ;;
(defvar.ps+ *entity-id-counter* 0)

(defstruct.ps+ ecs-entity
  (id (incf *entity-id-counter*))
  (components nil)
  parent
  (children '()))

(defvar.ps+ *entity-list* '())

(defun.ps+ clean-ecs-entities ()
  (setf *entity-list* '()))

(defun.ps+ process-all-entities (func)
  (labels ((rec (entity)
             (unless (null entity)
               (funcall func entity)
               (dolist (child (ecs-entity-children entity))
                 (rec child)))))
    (dolist (entity *entity-list*)
      (rec entity))))

#|
(pse:with-use-ps-pack (:this))
|#

;; ---- system ---- ;;
(defvar.ps+ *ecs-system-list* (make-hash-table))

(defstruct.ps+ ecs-system
  (enable t)
  (target-entities '())
  (target-component-types '())
  (process (lambda ())))

(defun.ps+ ecs-main ()
  (dolist (system *ecs-system-list*)
    (when (ecs-system-enable system)
      (funcall (ecs-system-process system)))))

(ps:ps (def-ecs-system (a (:include ecs-system)) c d))

;; ---- independent ---- ;;

(defun.ps+ includes-all-component-types (target-component-types components)
  (every (lambda (type)
           (some (lambda (comp)
                   (typep comp type))
                 components))
         target-component-types))

;; ---- Cross cutting ---- ;;

;; entity system

;; [WIP]
;; TODO: regester to system
(defun.ps+ add-ecs-entity (entity &optional (parent nil))
  (unless (ecs-entity-p entity)
    (error 'type-error :expected-type 'ecs-entity :datum entity))
  (if (null parent)
      (push entity *entity-list*)
      (progn (setf (ecs-entity-parent entity) parent)
             (push entity (ecs-entity-children parent)))))

;; [WIP]
(defun.ps+ remove-ecs-entity (entity))


;; [WIP]
(defmacro.ps def-ecs-system ((name &rest options) &rest slot-description)
  (unless (some (lambda (opt) (eq (car opt) :include)) options)
    (error "Error options: ~A must include ecs-system or its child" options))
  (unless (symbolp name)
    (error 'type-error :expected-type 'symbol :datum name))
  `(progn (defstruct ,(list* name options) ,@slot-description)
          (let ((obj ,(symbolicate 'make- name)))
            (setf (gethash ,name *ecs-system-list*) obj)
            ;; TODO: entity loop
            )))

#|
(def-ecs-system (a (:include b) (:test a)) c)
(ps:ps (def-ecs-system (a (:include ecs-system) (:test a)) c))
|#

;; entity component system

(defun.ps+ add-ecs-component (entity component)
  )

(defun.ps+ remove-ecs-component (entity component))

