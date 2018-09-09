(defpackage cl-ps-ecs/ecs
  (:use :cl
        :parenscript
        :ps-experiment
        :cl-ps-ecs/utils
        :cl-ps-ecs/basic-process
        :cl-ps-ecs/flat-tree)
  (:export :includes-all-component-types
           :ecs-component
           :find-a-component
           
           :make-ecs-entity
           :ecs-entity
           :ecs-entity-p
           :ecs-entity-id
           :ecs-entity-parent
           :get-ecs-component
           :with-ecs-components
           :with-ecs-entity-parent
           :add-ecs-entity
           :add-ecs-entity-to-buffer
           :delete-ecs-entity
           :do-ecs-entities
           :find-a-entity
           :find-the-entity
           :add-ecs-component
           :add-ecs-component-list
           :delete-ecs-component
           :delete-ecs-component-type

           :add-entity-tag
           :has-entity-tag
           :check-entity-tags
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

           :add-delete-component-hook
           :delete-delete-component-hook
           
           :do-ecs-components-of-entity
           :register-ecs-system
           :ecs-main
           :clean-ecs-env)
  (:import-from :alexandria
                :symbolicate
                :with-gensyms))
(in-package :cl-ps-ecs/ecs)

;; ---- component ---- ;;
(defstruct.ps+ (ecs-component (:include flat-tree-node)))

(defun.ps+ find-a-component (predicate top-component)
  "Find a component from top-component and its descendant by predicate"
  (check-type top-component ecs-component)
  (do-flat-tree (comp top-component)
    (when (funcall predicate comp)
      (return-from find-a-component comp)))
  nil)

;; ---- entity ---- ;;
(defvar.ps+ *entity-id-counter* 0)

(defstruct.ps+ (ecs-entity (:include flat-tree-node))
  (id (incf *entity-id-counter*))
  (tags '())
  (components '()))

(defvar.ps+ *entity-list* '())

(defun.ps+ get-entity-list ()
  *entity-list*)

(defmacro.ps+ do-ecs-entity-tree ((var top-entity) &body body)
  `(do-flat-tree (,var ,top-entity)
     ,@body))

(defmacro.ps+ do-ecs-entities (var &body body)
  `(do-flat-tree-list (,var (get-entity-list))
     ,@body))

(defun.ps+ clean-ecs-entities ()
  ;; TODO: Replace implementation by clean-flat-tree-list (not implemented yet)
  (do-ecs-entities entity
    (setf (ecs-entity-registerp entity) nil))
  (setf *entity-list* '()))

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
  (if (ecs-entity-registerp entity)
      entity
      nil))

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

(defun.ps+ check-entity-tags (entity &rest tags)
  (dolist (tag tags)
    (unless (has-entity-tag entity tag)
      (error "The entity has not a tag '~A'." tag)))
  t)

(defun.ps+ find-a-entity-by-tag (tag)
  (check-type tag string)
  (find-a-entity (lambda (entity)
                   (has-entity-tag entity tag))))

(defmacro.ps+ do-tagged-ecs-entities ((var tag) &body body)
  `(do-ecs-entities ,var
     (when (has-entity-tag ,var ,tag)
       ,@body)))

;; ---- system ---- ;;
(defvar.ps+ *ecs-system-list* '()
  "An element of the list is a pair of (name system)")

(defun.ps+ get-ecs-system-list ()
  *ecs-system-list*)

(defun.ps+ clean-ecs-systems ()
  (setf *ecs-system-list* '()))

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
  (with-gensyms (pair)
    (if (atom var)
        `(dolist (,pair (get-ecs-system-list))
           (let ((,var (cadr ,pair)))
             ,@body))
        `(dolist (,pair (get-ecs-system-list))
           (let ((,(car var) (car ,pair))
                 (,(cadr var) (cadr ,pair)))
             ,@body)))))

(defun.ps+ ecs-main ()
  (execute-ecs-basic-process)
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

(defun.ps+ is-registered-entity (entity system)
  (find entity (ecs-system-target-entities system)))

(defun.ps+ is-target-entity (entity system)
  (includes-all-component-types (ecs-system-target-component-types system)
                                (ecs-entity-components entity)))

;; entity component system

(defun.ps+ clean-ecs-env ()
  (clean-ecs-basic-process)
  (clean-ecs-entities)
  (clean-ecs-systems)
  (setf *delete-component-hooks* '()))

(defun.ps+ push-entity-to-system-if-needed (entity system)
  (when (is-target-entity entity system)
    (funcall (ecs-system-add-entity-hook system) entity)
    (pushnew entity (ecs-system-target-entities system))))

(defun.ps+ push-entity-to-all-target-system (entity)
  (do-ecs-systems system
    (push-entity-to-system-if-needed entity system)))

(defun.ps+ delete-entity-from-system-if-registered (entity system)
  (when (is-registered-entity entity system)
    (funcall (ecs-system-delete-entity-hook system) entity)
    (setf (ecs-system-target-entities system)
          (remove entity (ecs-system-target-entities system)))))

(defun.ps+ delete-entity-from-all-systems (entity)
  (do-ecs-systems system
    (delete-entity-from-system-if-registered entity system)))

(defun.ps+ delete-entity-from-no-longer-belong-systems (entity)
  (do-ecs-systems system
    (unless (is-target-entity entity system)
      (delete-entity-from-system-if-registered entity system))))

(defvar.ps+ *default-ecs-entity-parent* nil)

;; Note: In JavaScript environment, setf to variable in other package can't work.
(defun.ps+ setf-default-ecs-entity-parent (parent)
  (setf *default-ecs-entity-parent* parent))

(defmacro.ps+ with-ecs-entity-parent ((parent) &body body)
  "Set a default parent for add-ecs-entity.
When leaving the with scope, default parent is reverted."
  (with-gensyms (old-parent new-parent)
    `(let ((,old-parent *default-ecs-entity-parent*)
           (,new-parent ,parent))
       (unwind-protect
            (progn (unless (find-the-entity ,new-parent)
                     (add-ecs-entity ,new-parent))
                   (setf-default-ecs-entity-parent ,new-parent)
                   ,@body)
         (setf-default-ecs-entity-parent ,old-parent)))))

(defun.ps+ add-ecs-entity (entity &optional (parent *default-ecs-entity-parent*))
  "Add the entity to the global list. Then push it and its descendatns to the system if they have target components."
  (check-type entity ecs-entity)
  (when parent
    (check-type parent ecs-entity))
  (push-flat-tree-node entity *entity-list* parent)
  (do-ecs-entity-tree (target entity)
    (push-entity-to-all-target-system target))
  entity)

(defun.ps+ add-ecs-entity-to-buffer (entity &optional (parent *default-ecs-entity-parent*))
  "Add the entity to the buffer of the global list. They are added in the loop, ecs-main. This is useful for adding entities in do-ecs-entities loop."
  (register-next-frame-func
   #'(lambda () (add-ecs-entity entity parent))))

(defun.ps+ delete-ecs-entity (entity)
  "Remove an entity from global *entity-list* with its descendants."
  (check-type entity ecs-entity)
  (setf *entity-list* (delete-flat-tree-node entity *entity-list*))
  (do-ecs-entity-tree (target entity)
    (delete-entity-from-all-systems target)))

;; [WIP]
(defun.ps+ move-ecs-entity (entity new-parent)
  ;; TDOO: error if has not registered
  ())

(defun.ps+ register-ecs-system (name system)
  (check-type system ecs-system)
  (let ((found (find-if #'(lambda (pair)
                            (string= (car pair) name))
                        *ecs-system-list*)))
    (if found
        (setf (cadr found) system)
        (setf *ecs-system-list*
              (append *ecs-system-list* (list (list name system))))))
  (setf (ecs-system-target-entities system) '())
  (do-ecs-entities entity
    (push-entity-to-system-if-needed entity system))
  system)

(defun.ps+ check-component-uniqueness (component entity)
  ;; TODO: Fix: The ps-experiment:find has a bag in a block (dolist).
  (when (find component (ecs-entity-components entity))
    (error "The component is already added to the entity.")))

(defun.ps+ add-ecs-component-list-impl (entity parent-component component-list)
  "Add components to an entity. If the entity is added to the environment, "
  (check-type entity ecs-entity)
  (when parent-component
    (check-type parent-component ecs-component))
  (dolist (component component-list)
    (check-type component ecs-component)
    (check-component-uniqueness component entity)
    (push-flat-tree-node component (ecs-entity-components entity)
                         parent-component))
  (when (find-the-entity entity)
    (push-entity-to-all-target-system entity)))

(defun.ps+ add-ecs-component-list (entity &rest component-list)
  "Add components to an entity. If the entity is added to the environment, "
  (add-ecs-component-list-impl entity nil component-list))

(defun.ps+ add-ecs-component (component entity &optional parent-component)
  "Add a component to an entity. If the entity is added to the environment, "
  (add-ecs-component-list-impl entity parent-component (list component)))

(defvar.ps+ *delete-component-hooks* '())

(defun.ps+ add-delete-component-hook (callback)
  (pushnew callback *delete-component-hooks*))

(defun.ps+ delete-delete-component-hook (callback)
  (let ((pre-length (length *delete-component-hooks*)))
    (setf *delete-component-hooks*
          (remove callback *delete-component-hooks*))
    (when (eq pre-length (length *delete-component-hooks*))
      (error "The delete-component hook has not been added."))))

(defun.ps+ delete-ecs-component-impl (predicate entity allow-no-deletion)
  (check-type entity ecs-entity)
  (with-slots ((lst components)) entity
    (let ((pre-length (length lst)))
      (setf lst (delete-flat-tree-node-if
                 predicate lst
                 (lambda (component)
                   (dolist (hook *delete-component-hooks*)
                     (funcall hook component)))))
      (when (and (not allow-no-deletion)
                 (= pre-length (length lst)))
        (error "The component has not been added."))))
  (delete-entity-from-no-longer-belong-systems entity))

(defun.ps+ delete-ecs-component (component entity)
  (delete-ecs-component-impl
   (lambda (target-component)
     (eq target-component component))
   entity nil))

(defun.ps+ delete-ecs-component-type (component-type entity)
  "Delete a component whose type is component-type"
  (delete-ecs-component-impl
   (lambda (target-component)
     (typep target-component component-type))
   entity t))
