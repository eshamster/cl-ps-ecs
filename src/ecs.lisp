(in-package :cl-user)
(defpackage cl-ps-ecs.ecs
  (:use :cl
        :parenscript
        :ps-experiment)
  (:export :includes-all-component-types
           :ecs-component
           :ecs-entity
           :add-ecs-entity
           :process-all-entities
           :find-a-entity
           :add-ecs-component
           :remove-ecs-component
           
           :ecs-system
           :target-component-types
           :process
           
           :register-ecs-system
           :ecs-main
           :clean-ecs-env)
  (:import-from :alexandria
                :symbolicate
                :with-gensyms))
(in-package :cl-ps-ecs.ecs)

;; ---- component ---- ;;
;; This has dummy data to avoid error in cl-js:js-run with Clozure CL
(defstruct.ps+ ecs-component
    dummy)

;; ---- entity ---- ;;
(defvar.ps+ *entity-id-counter* 0)

(defstruct.ps+ ecs-entity
  (id (incf *entity-id-counter*))
  (components '())
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

(defun.ps+ find-a-entity (predicate)
  (process-all-entities
   (lambda (entity)
     (if (funcall predicate entity)
         (return-from find-a-entity entity))))
  nil)

;; ---- system ---- ;;
(defvar.ps+ *ecs-system-hash* (make-hash-table))

(defun.ps+ clean-ecs-systems ()
  (setf *ecs-system-hash* (make-hash-table)))

(defstruct.ps+ ecs-system
  (enable t)
  (target-entities '()) ;; automatically updated
  (target-component-types '())
  (process (lambda (entity) entity)))

(defmacro.ps+ do-ecs-systems (var &body body)
  (if (atom var)
      (with-gensyms (name)
        `(maphash (lambda (,name ,var)
                    ,@body)
                  *ecs-system-hash*))
      `(maphash (lambda (,(car var) ,(cadr var))
                  ,@body)
                *ecs-system-hash*)))

(defun.ps+ ecs-main ()
  (do-ecs-systems system
    (when (ecs-system-enable system)
      (dolist (entity (ecs-system-target-entities system))
        (funcall (ecs-system-process system) entity)))))

;; ---- independent ---- ;;

(defun.ps+ includes-all-component-types (target-component-types components)
  (every (lambda (type)
           (some (lambda (comp)
                   (typep comp type))
                 components))
         target-component-types))

;; ---- Cross cutting ---- ;;

;; entity system

(defun.ps+ is-target-entity (entity system)
  (includes-all-component-types (ecs-system-target-component-types system)
                                (ecs-entity-components entity)))

;; entity component system

(defun.ps+ clean-ecs-env ()
  (clean-ecs-entities)
  (clean-ecs-systems))

;; [WIP]
;; TODO: regester to system
(defun.ps+ add-ecs-entity (entity &optional (parent nil))
  (unless (ecs-entity-p entity)
    (error 'type-error :expected-type 'ecs-entity :datum entity))
  (if (null parent)
      (push entity *entity-list*)
      (progn (setf (ecs-entity-parent entity) parent)
             (push entity (ecs-entity-children parent))))
  entity)

;; [WIP]
(defun.ps+ remove-ecs-entity (entity))

(defun.ps+ register-ecs-system (name system)
  (unless (ecs-system-p system)
    (error 'type-error :expected-type 'ecs-system :datum system))
  (setf (gethash name *ecs-system-hash*) system)
  (setf (ecs-system-target-entities system) '())
  (process-all-entities
   (lambda (entity)
     (when (is-target-entity entity system)
       (push entity (ecs-system-target-entities system)))))
  system)

;; [WIP]
(defun.ps+ add-ecs-component (component entity)
  (unless (ecs-component-p component)
    (error 'type-error :expected-type 'ecs-component :datum component))
  (unless (ecs-entity-p entity)
    (error 'type-error :expected-type 'ecs-entity :datum entity))
  (push component (ecs-entity-components entity))
  ;; TODO: regester to system only if the entity is registered to global
  )

;; [WIP]
(defun.ps+ remove-ecs-component (component entity))

