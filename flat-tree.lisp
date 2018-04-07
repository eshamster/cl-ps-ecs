(in-package :cl-user)
(defpackage cl-ps-ecs/flat-tree
  (:use :cl
        :parenscript
        :ps-experiment)
  (:import-from :alexandria
                :with-gensyms)
  (:export :flat-tree-node
           :make-flat-tree-node
           :flat-tree-node-p
           :flat-tree-node-parent
           :flat-tree-node-children
           :flat-tree-node-registerp

           :push-flat-tree-node
           :do-flat-tree
           :do-flat-tree-list
           :delete-flat-tree-node
           :delete-flat-tree-node-if))
(in-package :cl-ps-ecs/flat-tree)

;; This package is internally used for ecs-entity and ecs-component.
;; The structure is required for them.
;; But all nodes in the trees are frequently processed.
;; So flat-tree can do such process by simple one dimentional loop
;; instead of recursive loop.

(defstruct.ps+ flat-tree-node
  parent
  (children '())
  (registerp nil))

;; TODO: Break off a relation with its parent when the node has a previous parent.
(defun.ps+ add-flat-tree-node (node lst &optional parent)
  "Add a flat-tree node to the lst.
Because this function can destruct the lst, caller should overwrite lst by new returned list."
  (check-type node flat-tree-node)
  (when (flat-tree-node-registerp node)
    (error "The node is already registered."))
  (when parent
    (check-type parent flat-tree-node)
    (unless (flat-tree-node-registerp parent)
      (error "The parent is not registered")))
  (when parent
    (setf (flat-tree-node-parent node) parent)
    (push node (flat-tree-node-children parent)))
  (labels ((rec (one-node)
             (unless (flat-tree-node-registerp one-node)
               (push one-node lst)
               (setf (flat-tree-node-registerp one-node) t)
               (dolist (child (flat-tree-node-children one-node))
                 (rec child)))))
    (rec node))
  lst)

(defmacro.ps+ push-flat-tree-node (node lst-place &optional parent)
  `(setf ,lst-place
        (add-flat-tree-node ,node ,lst-place ,parent)))

(defmacro.ps+ do-flat-tree ((var flat-tree-node) &body body)
  (with-gensyms (rec)
    `(progn
       (check-type ,flat-tree-node flat-tree-node)
       (labels ((,rec (,var)
                  ,@body
                  (dolist (child (flat-tree-node-children ,var))
                    (,rec child))))
         (,rec ,flat-tree-node)))))

(defmacro.ps+ do-flat-tree-list ((var lst) &body body)
  `(dolist (,var ,lst)
     ,@body))

(defun.ps+ exec-nothing (&rest _)
  (declare (ignore _)))

(defun.ps+ delete-flat-tree-node (node place-lst &optional (callback #'exec-nothing))
  "Delete a flat-tree node from the place-lst.
Because this function destruct the place-lst, caller should overwrite place-lst by new returned list."
  (check-type node flat-tree-node)
  ;; Break off a relation with its parent
  (unless (flat-tree-node-registerp node)
    (error "Can't delete a not-registered flat-node"))
  (with-slots (parent children) node
    (when parent
      (setf (flat-tree-node-children parent)
            (remove node (flat-tree-node-children parent)))
      (setf parent nil)))
  ;; Keep relations with its descendent
  (labels ((rec (one-node lst)
             (if (flat-tree-node-registerp one-node)
                 (let ((removed-lst (remove one-node lst)))
                   (funcall callback one-node)
                   (setf (flat-tree-node-registerp one-node) nil)
                   (dolist (child (flat-tree-node-children one-node))
                     (setf removed-lst (rec child removed-lst)))
                   removed-lst)
                 lst)))
    (rec node place-lst)))

(defun.ps+ delete-flat-tree-node-if (predicate place-lst &optional (callback #'exec-nothing))
  "Delete a flat-tree node that becomes true in the predicate from the place-lst.
Because this function destruct the place-lst, caller should overwrite place-lst by new returned list.
Note: When parent and child are deleted at the same time, it is not guaranteed that their relationship is kept or not."
  (let ((delete-lst '()))
    (do-flat-tree-list (node place-lst)
      (when (funcall predicate node)
        (push node delete-lst)))
    ;; Note: Child nodes can be recursively deleted when its parent is deleted,
    ;;       so registerp should be checked before deletion.
    (dolist (node delete-lst)
      (when (flat-tree-node-registerp node)
        (setf place-lst (delete-flat-tree-node node place-lst callback)))))
  place-lst)
