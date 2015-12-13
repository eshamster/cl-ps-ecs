(in-package :cl-user)
(defpackage cl-ps-ecs.ecs
  (:use :cl
        :parenscript
        :ps-experiment)
  (:export :includes-all-component-types
           :ecs-component
           
           :ecs-entity
           :add-ecs-entity
           :delete-ecs-entity
           :do-ecs-entities
           :find-a-entity
           :find-the-entity
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

(defmacro.ps+ do-ecs-entity-tree ((var top-entity) &body body)
  (with-gensyms (rec)
    `(labels ((,rec (,var)
                (unless (null ,var)
                  ,@body
                  (dolist (child (ecs-entity-children ,var))
                    (,rec child)))))
       (,rec ,top-entity))))

(defmacro.ps+ do-ecs-entities (var &body body)
  `(dolist (entity *entity-list*)
     (do-ecs-entity-tree (,var entity)
       ,@body)))

(defun.ps+ find-a-entity (predicate)
  "Find a registered entity by predicate"
  (do-ecs-entities entity
    (if (funcall predicate entity)
        (return-from find-a-entity entity)))
  nil)

(defun.ps+ find-the-entity (entity)
  "Find a registered entity by comparing the address"
  (find-a-entity
   (lambda (target) (eq entity target))))

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
                    (declare (ignore ,name))
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

(defun.ps+ push-entity-to-all-target-system (entity)
  (do-ecs-systems system
    (when (is-target-entity entity system)
      (push entity (ecs-system-target-entities system)))))

(defun.ps+ add-ecs-entity (entity &optional (parent nil))
  (unless (ecs-entity-p entity)
    (error 'type-error :expected-type 'ecs-entity :datum entity))
  (when (find-the-entity entity)
    (error "The entity is already registered."))
  (if (null parent)
      (push entity *entity-list*)
      (progn (setf (ecs-entity-parent entity) parent)
             (push entity (ecs-entity-children parent))))
  (push-entity-to-all-target-system entity)
  entity)

(defun.ps+ delete-ecs-entity (entity)
  "Remove an entity from global *entity-list* with its descendants."
  (unless (ecs-entity-p entity)
    (error 'type-error :expected-type 'ecs-entity :datum entity))
  (unless (find-the-entity entity)
    (error "The entity is not registered"))
  (let ((parent (ecs-entity-parent entity)))
    (if parent
        (progn (setf (ecs-entity-children parent)
                     (remove entity (ecs-entity-children parent)))
               (setf (ecs-entity-parent entity) nil))
        (setf *entity-list*
              (remove entity *entity-list*)))))

;; [WIP]
(defun.ps+ move-ecs-entity (entity new-parent)
  ;; TDOO: error if has not registered
  ())

(defun.ps+ register-ecs-system (name system)
  (unless (ecs-system-p system)
    (error 'type-error :expected-type 'ecs-system :datum system))
  (setf (gethash name *ecs-system-hash*) system)
  (setf (ecs-system-target-entities system) '())
  (do-ecs-entities entity
    (when (is-target-entity entity system)
      (push entity (ecs-system-target-entities system))))
  system)

(defun.ps+ add-ecs-component (component entity)
  "Add a component to an entity. If the entity is added to the environment, "
  (unless (ecs-component-p component)
    (error 'type-error :expected-type 'ecs-component :datum component))
  (unless (ecs-entity-p entity)
    (error 'type-error :expected-type 'ecs-entity :datum entity))
  (when (find component (ecs-entity-components entity))
    (error "The component is already added to the entity."))
  (push component (ecs-entity-components entity))
  (when (find-the-entity entity)
    (push-entity-to-all-target-system entity)))

;; [WIP]
(defun.ps+ remove-ecs-component (component entity))

