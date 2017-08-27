(in-package :cl-user)
(defpackage cl-ps-ecs-test.flat-tree
  (:use :cl
        :cl-ps-ecs.flat-tree
        :parenscript
        :ps-experiment
        :prove
        :ps-experiment-test.test-utils))
(in-package :cl-ps-ecs-test.flat-tree)

(plan 7)

(declaim #+sbcl (sb-ext:muffle-conditions sb-ext:compiler-note))

;; --- Prepare --- ;;

(defun.ps+ is-flat-tree (tree &key count)
  (unless (= (length tree) count)
    (format t "    The count is wrong (got: ~D, expected: ~D)" (length tree) count)
    (return-from is-flat-tree nil))
  (unless (every (lambda (node) (flat-tree-node-p node)) tree)
    (format t "    Some node is not the type, \"flat-tree-node\"")
    (return-from is-flat-tree nil))
  (unless (every (lambda (node) (not (null (flat-tree-node-registerp node)))) tree)
    (format t "    Registerp is nil in some node")
    (return-from is-flat-tree nil))
  t)

(defun.ps+ is-parent-node (node expected-parent)
  (check-type node flat-tree-node)
  (check-type expected-parent flat-tree-node)
  (let ((is-parent (eq (flat-tree-node-parent node) expected-parent))
        (is-child (find node (flat-tree-node-children expected-parent))))
    (when (or (and is-parent (not is-child))
              (and (not is-parent) is-child))
      (error "The relation between child and parent is one directional."))
    (and is-child is-parent)))

;; The next subtest avoids using exported functions as far as possible (because they
;; have not been tested yet) and directly set flat-tree-node parameters instead.
;; Of cource, this is not recommended in ordinal use cases.
(subtest
    "Test utilities for test"
  (subtest
      "is-flat-tree"
    (with-prove-in-both ()
      ;; wrong count
      (ok (is-flat-tree '() :count 0))
      (ok (not (is-flat-tree '() :count 2)))
      ;; including not flat-tree-node
      (let ((node (make-flat-tree-node :registerp t))
            (lst '()))
        (push node lst)
        (ok (is-flat-tree lst :count 1))
        (push 1 lst)
        (is (length lst) 2)
        (ok (not (is-flat-tree lst :count 2))))
      ;; including not registered
      (let* ((node1 (make-flat-tree-node :registerp t))
             (node2 (make-flat-tree-node :registerp t))
             (node3 (make-flat-tree-node))
             (lst '()))
        (push node1 lst)
        (ok (is-flat-tree lst :count 1))
        (push node2 lst)
        (ok (is-flat-tree lst :count 2))
        (push node3 lst)
        (is (length lst) 3)
        (ok (not (is-flat-tree lst :count 3))))))
  (subtest
      "is-parent-node"
    (with-prove-in-both ()
      (let ((child (make-flat-tree-node))
            (parent (make-flat-tree-node)))
        ;; ng
        (ok (not (is-parent-node child parent)))
        ;; ok
        (setf (flat-tree-node-parent child) parent)
        (push child (flat-tree-node-children parent))
        (ok (is-parent-node child parent))
        ;; error case 1
        (setf (flat-tree-node-parent child) nil)
        (is-error (is-parent-node child parent) 'simple-error)
        ;; error case 2
        (setf (flat-tree-node-parent child) parent)
        (ok (is-parent-node child parent))
        (setf (flat-tree-node-children parent) '())
        (is-error (is-parent-node child parent) 'simple-error)))))

;; --- Start test --- ;;

(subtest
    "Test push-flat-tree-node"
  (subtest "Normal case"
    (with-prove-in-both ()
      (let ((parent (make-flat-tree-node))
            (child (make-flat-tree-node))
            (lst '()))
        (push-flat-tree-node parent lst)
        (push-flat-tree-node child lst parent)
        (ok (is-flat-tree lst :count 2))
        (ok (is-parent-node child parent))))
    (ok "Note: Recursive pushing is tested in the subtest \"Test push and delete\""))
  (subtest "Error case"
    (subtest "Wrong type"
      (with-prove-in-both ()
        (let ((lst '()))
          (is-error (push-flat-tree-node :not-node lst) 'type-error)
          (is-error (push-flat-tree-node (make-flat-tree-node) lst 1)
                    'type-error))))
    (subtest "Parent is not registered"
      (with-prove-in-both ()
        (let ((parent (make-flat-tree-node))
              (child (make-flat-tree-node))
              (lst '()))
          (is-error (push-flat-tree-node child lst parent)
                    'simple-error))))
    (subtest "Duplicated registration"
      (with-prove-in-both ()
        (let ((lst '())
              (node (make-flat-tree-node)))
          (ok (push-flat-tree-node node lst))
          (is-error (push-flat-tree-node node lst)
                    'simple-error))))))

(subtest
    "Test delete-flat-node"
  (subtest "Normal case"
    (with-prove-in-both ()
      (let ((parent (make-flat-tree-node))
            (child (make-flat-tree-node))
            (lst '()))
        (push-flat-tree-node parent lst)
        (push-flat-tree-node child lst parent)
        (ok (is-flat-tree lst :count 2))
        ;; delete child
        (setf lst (delete-flat-tree-node child lst))
        (ok (is-flat-tree lst :count 1))
        (ok (not (is-parent-node child parent)))
        ;; delete parent
        (push-flat-tree-node child lst parent)
        (ok (is-flat-tree lst :count 2))
        (setf lst (delete-flat-tree-node parent lst))
        (ok (is-flat-tree lst :count 0))
        (ok (is-parent-node child parent)))))
  (subtest "Error case"
    (subtest "Wrong type"
      (with-prove-in-both ()
        (let ((lst '()))
          (is-error (delete-flat-tree-node :not-node lst) 'type-error))))
    (subtest "Duplicated deletion"
      (with-prove-in-both ()
        (let ((lst '())
              (node (make-flat-tree-node)))
          (ok (push-flat-tree-node node lst))
          (setf lst (delete-flat-tree-node node lst))
          (ok (is-flat-tree lst :count 0))
          (is-error (setf lst (delete-flat-tree-node node lst))
                    'simple-error))))))

(subtest
    "Test delete-flat-tree-node-if"
  (with-prove-in-both ()
    (let ((parent (make-flat-tree-node))
          (child1 (make-flat-tree-node))
          (child2 (make-flat-tree-node))
          (g-child (make-flat-tree-node))
          (lst '()))
      (print "  --- prepare ---")
      (push-flat-tree-node (make-flat-tree-node) lst)
      (push-flat-tree-node parent lst)
      (push-flat-tree-node child1 lst parent)
      (push-flat-tree-node child2 lst parent)
      (push-flat-tree-node g-child lst child1)
      (ok (is-flat-tree lst :count 5))
      (ok (is-parent-node child1 parent))
      (ok (is-parent-node child2 parent))
      (ok (is-parent-node g-child child1))

      (print "  --- delete ---")
      (setf lst
            (delete-flat-tree-node-if (lambda (node)
                                        (check-type node flat-tree-node)
                                        (is-parent-node node parent))
                                      lst))
      (ok (is-flat-tree lst :count 2)))))

(subtest
    "Test push and delete"
  (with-prove-in-both ()
    (let ((parent (make-flat-tree-node))
          (child1 (make-flat-tree-node))
          (child2 (make-flat-tree-node))
          (g-child (make-flat-tree-node))
          (lst '()))
      (print "  --- prepare ---")
      (push-flat-tree-node (make-flat-tree-node) lst)
      (push-flat-tree-node parent lst)
      (push-flat-tree-node child1 lst parent)
      (push-flat-tree-node child2 lst parent)
      (push-flat-tree-node g-child lst child1)
      (ok (is-flat-tree lst :count 5))
      (ok (is-parent-node child1 parent))
      (ok (is-parent-node child2 parent))
      (ok (is-parent-node g-child child1))

      (print "  --- delete an intermidiate node ---")
      (setf lst (delete-flat-tree-node child1 lst))
      (ok (is-flat-tree lst :count 3))
      (ok (not (is-parent-node child1 parent)))
      (ok (is-parent-node child2 parent))
      (ok (is-parent-node g-child child1))

      (print "  --- push the deleted child again (g-child should also be pushed) ---")
      (push-flat-tree-node child1 lst parent)
      (ok (is-flat-tree lst :count 5))
      (ok (is-parent-node child1 parent))
      (ok (is-parent-node child2 parent))
      (ok (is-parent-node g-child child1))

      (print "  --- delete a top node (to check recursive deletion more than 2 depth) ---")
      (setf lst (delete-flat-tree-node parent lst))
      (ok (is-flat-tree lst :count 1))
      (ok (is-parent-node child1 parent))
      (ok (is-parent-node child2 parent))
      (ok (is-parent-node g-child child1))

      (print "  --- push a top node (to check recursive pushing more than 2 depth) ---")
      (push-flat-tree-node parent lst)
      (ok (is-flat-tree lst :count 5))
      (ok (is-parent-node child1 parent))
      (ok (is-parent-node child2 parent))
      (ok (is-parent-node g-child child1)))))

(subtest
    "Test do-flat-tree"
  (subtest "Normal case"
    (with-prove-in-both ()
      (let ((parent (make-flat-tree-node))
            (lst '())
            (count 0))
        (push-flat-tree-node parent lst)
        (push-flat-tree-node (make-flat-tree-node) lst parent)
        (push-flat-tree-node (make-flat-tree-node) lst)
        (ok (is-flat-tree lst :count 3))
        (do-flat-tree (node parent)
          (check-type node flat-tree-node)
          (incf count))
        (is count 2))))
  (subtest "Error case"
    (subtest "Type error"
      (with-prove-in-both ()
        (let ((not-flat-tree-node 1))
          (is-error (do-flat-tree (node not-flat-tree-node)
                      (print node))
                    'type-error))))))

(subtest
    "Test do-flat-tree-list"
  (subtest "Normal case"
    (with-prove-in-both ()
      (let ((parent (make-flat-tree-node))
            (lst '())
            (count 0))
        (push-flat-tree-node parent lst)
        (push-flat-tree-node (make-flat-tree-node) lst parent)
        (push-flat-tree-node (make-flat-tree-node) lst)
        (ok (is-flat-tree lst :count 3))
        (do-flat-tree-list (node lst)
          (check-type node flat-tree-node)
          (incf count))
        (is count 3)))))

(finalize)
