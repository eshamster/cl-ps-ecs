(defpackage cl-ps-ecs/t/flat-tree
  (:use :cl
        :cl-ps-ecs/flat-tree
        :parenscript
        :ps-experiment
        :rove
        :ps-experiment/t/test-utils))
(in-package :cl-ps-ecs/t/flat-tree)

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
(deftest.ps+ verify-test-utils
  (testing "is-flat-tree"
    ;; wrong count
    (ok (is-flat-tree '() :count 0))
    (ok (not (is-flat-tree '() :count 2)))
    ;; including not flat-tree-node
    (let ((node (make-flat-tree-node :registerp t))
          (lst '()))
      (push node lst)
      (ok (is-flat-tree lst :count 1))
      (push 1 lst)
      (ok (= (length lst) 2))
      (ng (is-flat-tree lst :count 2)))
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
      (ok (= (length lst) 3))
      (ng (is-flat-tree lst :count 3))))
  (testing "is-parent-node"
    (let ((child (make-flat-tree-node))
          (parent (make-flat-tree-node)))
      ;; ng
      (ng (is-parent-node child parent))
      ;; ok
      (setf (flat-tree-node-parent child) parent)
      (push child (flat-tree-node-children parent))
      (ok (is-parent-node child parent))
      ;; error case 1
      (setf (flat-tree-node-parent child) nil)
      (ok (signals (is-parent-node child parent) 'simple-error))
      ;; error case 2
      (setf (flat-tree-node-parent child) parent)
      (ok (is-parent-node child parent))
      (setf (flat-tree-node-children parent) '())
      (ok (signals (is-parent-node child parent) 'simple-error)))))

;; --- Start test --- ;;

(deftest.ps+ for-push-flat-tree-node
  (testing "Normal case"
    (let ((parent (make-flat-tree-node))
          (child (make-flat-tree-node))
          (lst '()))
      (push-flat-tree-node parent lst)
      (push-flat-tree-node child lst parent)
      (ok (is-flat-tree lst :count 2))
      (ok (is-parent-node child parent)))
    (ok "Note: Recursive pushing is tested in the testing \"Test push and delete\""))
  (testing "Error case"
    (testing "Wrong type"
      (let ((lst '()))
        (ok (signals (push-flat-tree-node :not-node lst) 'type-error))
        (ok (signals (push-flat-tree-node (make-flat-tree-node) lst 1)
                     'type-error))))
    (testing "Parent is not registered"
      (let ((parent (make-flat-tree-node))
            (child (make-flat-tree-node))
            (lst '()))
        (ok (signals (push-flat-tree-node child lst parent)
                     'simple-error))))
    (testing "Duplicated registration"
      (let ((lst '())
            (node (make-flat-tree-node)))
        (ok (push-flat-tree-node node lst))
        (ok (signals (push-flat-tree-node node lst)
                     'simple-error))))))

(deftest.ps+ for-delete-flat-node
  (testing "Normal case"
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
      (ok (is-parent-node child parent))))
  (testing "Hook"
    (let ((parent (make-flat-tree-node))
          (child (make-flat-tree-node))
          (g-child (make-flat-tree-node))
          (lst '())
          (count 0))
      (push-flat-tree-node parent lst)
      (push-flat-tree-node child lst parent)
      (push-flat-tree-node g-child lst child)
      (ok (is-flat-tree lst :count 3))
      ;; delete child
      (setf lst (delete-flat-tree-node
                 child lst
                 (lambda (node)
                   (cond ((eq node child) (incf count 1))
                         ((eq node g-child) (incf count 10))
                         (t (error "The node should not be deleted"))))))
      (ok (is-flat-tree lst :count 1))
      (ok (= count 11))))
  (testing "Error case"
    (testing "Wrong type"
      (let ((lst '()))
        (ok (signals (delete-flat-tree-node :not-node lst) 'type-error))))
    (testing "Duplicated deletion"
      (let ((lst '())
            (node (make-flat-tree-node)))
        (ok (push-flat-tree-node node lst))
        (setf lst (delete-flat-tree-node node lst))
        (ok (is-flat-tree lst :count 0))
        (ok (signals (setf lst (delete-flat-tree-node node lst))
                     'simple-error))))))

(deftest.ps+ for-delete-flat-tree-node-if
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
    (ok (is-flat-tree lst :count 2))))

(deftest.ps+ for-push_and_delete
  (let ((parent (make-flat-tree-node))
        (child1 (make-flat-tree-node))
        (child2 (make-flat-tree-node))
        (g-child (make-flat-tree-node))
        (lst '()))
    (testing "prepare"
      (push-flat-tree-node (make-flat-tree-node) lst)
      (push-flat-tree-node parent lst)
      (push-flat-tree-node child1 lst parent)
      (push-flat-tree-node child2 lst parent)
      (push-flat-tree-node g-child lst child1)
      (ok (is-flat-tree lst :count 5))
      (ok (is-parent-node child1 parent))
      (ok (is-parent-node child2 parent))
      (ok (is-parent-node g-child child1)))
    (testing "delete an intermidiate node"
      (setf lst (delete-flat-tree-node child1 lst))
      (ok (is-flat-tree lst :count 3))
      (ok (not (is-parent-node child1 parent)))
      (ok (is-parent-node child2 parent))
      (ok (is-parent-node g-child child1)))
    (testing "push the deleted child again (g-child should also be pushed)"
      (push-flat-tree-node child1 lst parent)
      (ok (is-flat-tree lst :count 5))
      (ok (is-parent-node child1 parent))
      (ok (is-parent-node child2 parent))
      (ok (is-parent-node g-child child1)))
    (testing "delete a top node (to check recursive deletion more than 2 depth)"
      (setf lst (delete-flat-tree-node parent lst))
      (ok (is-flat-tree lst :count 1))
      (ok (is-parent-node child1 parent))
      (ok (is-parent-node child2 parent))
      (ok (is-parent-node g-child child1)))
    (testing "push a top node (to check recursive pushing more than 2 depth)"
      (push-flat-tree-node parent lst)
      (ok (is-flat-tree lst :count 5))
      (ok (is-parent-node child1 parent))
      (ok (is-parent-node child2 parent))
      (ok (is-parent-node g-child child1)))))

(deftest.ps+ for-do-flat-tree
  (testing "Normal case"
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
      (ok (= count 2))))
  (testing "Error case"
    (testing "Type error"
      (let ((not-flat-tree-node 1))
        (ok (signals (do-flat-tree (node not-flat-tree-node)
                       (print node))
                     'type-error))))))

(deftest.ps+ for-do-flat-tree-list
  (testing "Normal case"
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
      (ok (= count 3)))))
