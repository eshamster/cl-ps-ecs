(in-package :cl-user)
(defpackage cl-ps-ecs.ecs
  (:use :cl
        :parenscript
        :ps-experiment
        :cl-ps-ecs.utils)
  (:export :includes-all-component-types
           :ecs-component
           
           :ecs-entity
           :ecs-entity-p
           :ecs-entity-id
           :ecs-entity-parent
           :get-ecs-component
           :with-ecs-components
           :add-ecs-entity
           :delete-ecs-entity
           :do-ecs-entities
           :find-a-entity
           :find-the-entity
           :add-ecs-component
           :remove-ecs-component

           :add-entity-tag
           :has-entity-tag
           :delete-entity-tag
           :find-a-entity-by-tag
           :do-tagged-ecs-entities
           
           :ecs-system
           :ecs-system-target-entities
           :target-entities
           :target-component-types
           :process
           :process-all
           :add-entity-hook
           :delete-entity-hook
           
           :do-ecs-components-of-entity
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
  (tags '())
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

(defun.ps+ get-ecs-component (component-type entity)
  "Get a component from entity by component-type"
  (find-if (lambda (component)
             (typep component component-type))
           (ecs-entity-components entity)))

(defmacro.ps+ with-ecs-components ((&rest target-elements) entity &body body)
  "Bind components of entity to the type name. Throw error if some components are not included in the entity"
  `(let ,(mapcar (lambda (element)
                   (let ((var (if (listp element) (car element) element))
                         (type (if (listp element) (cadr element) element)))
                     `(,var (let ((found (get-ecs-component ',type ,entity)))
                              (if found
                                  found
                                  (error ,(format nil "~A is not included in the entity" type)))))))
                 target-elements) 
     ,@body))

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

;; - about tag - ;;

(defun.ps+ add-entity-tag (entity &rest tags)
  (check-type entity ecs-entity)
  (dolist (tag tags)
    (check-type tag string)
    (push tag (ecs-entity-tags entity))))

(defun.ps+ delete-entity-tag (entity tag)
  (check-type entity ecs-entity)
  (check-type tag string)
  (setf (ecs-entity-tags entity)
        (remove-if (lambda (target-tag)
                     (string= target-tag tag))
                   (ecs-entity-tags entity))))

(defun.ps+ has-entity-tag (entity tag)
  (check-type entity ecs-entity)
  (check-type tag string)
  (find-if (lambda (target-tag)
             (string= target-tag tag))
           (ecs-entity-tags entity)))

(defun.ps+ find-a-entity-by-tag (tag)
  (check-type tag string)
  (find-a-entity (lambda (entity)
                   (has-entity-tag entity tag))))

(defmacro.ps+ do-tagged-ecs-entities ((var tag) &body body)
  `(do-ecs-entities ,var
     (when (has-entity-tag ,var ,tag)
       ,@body)))

;; ---- system ---- ;;
(defvar.ps+ *ecs-system-hash* (make-hash-table))

(defun.ps+ clean-ecs-systems ()
  (setf *ecs-system-hash* (make-hash-table)))

(defstruct.ps+ ecs-system
  (enable t)
  (target-entities '()) ;; automatically updated
  (target-component-types '())
  (process (lambda (entity) entity)) ;; process each entity
  (process-all (lambda (system) system))
  (add-entity-hook (lambda (entity) entity))
  (delete-entity-hook (lambda (entity) entity)))

(defmacro.ps+ do-ecs-systems (var &body body)
  "Iterates all registered ecs-system. If need only the object of each system, write as (do-ecs-systems sys ...). If need also the registered name, write as (do-ecs-system (name sys) ...)."
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
      (funcall (ecs-system-process-all system) system)
      (dolist (entity (ecs-system-target-entities system))
        (funcall (ecs-system-process system) entity)))))

;; ---- Cross cutting ---- ;;

;; entity component

(defmacro.ps+ do-ecs-components-of-entity ((var entity &key component-type) &body body)
  `(dolist (,var (ecs-entity-components ,entity))
     ,@(if component-type
           `((when (typep ,var ,component-type)
               ,@body))
           body)))

;; entity system

(defun.ps+ is-target-entity (entity system)
  (includes-all-component-types (ecs-system-target-component-types system)
                                (ecs-entity-components entity)))

;; entity component system

(defun.ps+ clean-ecs-env ()
  (clean-ecs-entities)
  (clean-ecs-systems))

(defun.ps+ push-entity-to-system-if-needed (entity system)
  (when (is-target-entity entity system)
    (funcall (ecs-system-add-entity-hook system) entity)
    (push entity (ecs-system-target-entities system))))

(defun.ps+ push-entity-to-all-target-system (entity)
  (do-ecs-systems system
    (push-entity-to-system-if-needed entity system)))

(defun.ps+ delete-entity-from-system-if-needed (entity system)
  (when (is-target-entity entity system)
    (funcall (ecs-system-delete-entity-hook system) entity)
    (setf (ecs-system-target-entities system)
          (remove entity (ecs-system-target-entities system)))))

(defun.ps+ add-ecs-entity (entity &optional (parent nil))
  "Add the entity to the global list. Then push it and its descendatns to the system if they have target components."
  (check-type entity ecs-entity)
  (when (find-the-entity entity)
    (error "The entity is already registered."))
  (when parent
    (check-type parent ecs-entity))
  (unless (or (null parent) (find-the-entity parent))
    (error "The parent is not registered"))
  (if (null parent)
      (push entity *entity-list*)
      (progn (setf (ecs-entity-parent entity) parent)
             (push entity (ecs-entity-children parent))))
  (do-ecs-entity-tree (target entity)
    (push-entity-to-all-target-system target))
  entity)

(defun.ps+ delete-ecs-entity (entity)
  "Remove an entity from global *entity-list* with its descendants."
  (check-type entity ecs-entity)
  (unless (find-the-entity entity)
    (error "The entity is not registered"))
  (let ((parent (ecs-entity-parent entity)))
    (if parent
        (progn (setf (ecs-entity-children parent)
                     (remove entity (ecs-entity-children parent)))
               (setf (ecs-entity-parent entity) nil))
        (setf *entity-list*
              (remove entity *entity-list*))))
  (do-ecs-entity-tree (target entity)
    (do-ecs-systems system
      (delete-entity-from-system-if-needed target system))))

;; [WIP]
(defun.ps+ move-ecs-entity (entity new-parent)
  ;; TDOO: error if has not registered
  ())

(defun.ps+ register-ecs-system (name system)
  (check-type system ecs-system)
  (setf (gethash name *ecs-system-hash*) system)
  (setf (ecs-system-target-entities system) '())
  (do-ecs-entities entity
    (push-entity-to-system-if-needed entity system))
  system)

(defun.ps+ add-ecs-component (component entity)
  "Add a component to an entity. If the entity is added to the environment, "
  (check-type component ecs-component)
  (check-type entity ecs-entity)
  (when (find component (ecs-entity-components entity))
    (error "The component is already added to the entity."))
  (push component (ecs-entity-components entity))
  (when (find-the-entity entity)
    (push-entity-to-all-target-system entity)))

;; [WIP]
(defun.ps+ delete-ecs-component (component entity))

