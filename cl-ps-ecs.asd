#|
  This file is a part of cl-ps-ecs project.
  Copyright (c) 2015 eshamster (hamgoostar@gmail.com)
|#

#|
  Author: eshamster (hamgoostar@gmail.com)
|#

(in-package :cl-user)
(defpackage cl-ps-ecs-asd
  (:use :cl :asdf))
(in-package :cl-ps-ecs-asd)

(defsystem cl-ps-ecs
  :version "0.1"
  :author "eshamster"
  :license "LLGPL"
  :depends-on (:parenscript
               :alexandria
               :ps-experiment
               :cl-reexport)
  :components ((:module "src"
                :serial t
                :components
                ((:file "utils")
                 (:file "flat-tree")
                 (:file "basic-process")
                 (:file "frame-promise")
                 (:file "ecs")
                 (:file "cl-ps-ecs"))))
  :description "[WIP] Entity-Component-System written by ParenScript (Common Lisp) "
  :long-description
  #.(with-open-file (stream (merge-pathnames
                             #p"README.markdown"
                             (or *load-pathname* *compile-file-pathname*))
                            :if-does-not-exist nil
                            :direction :input)
      (when stream
        (let ((seq (make-array (file-length stream)
                               :element-type 'character
                               :fill-pointer t)))
          (setf (fill-pointer seq) (read-sequence seq stream))
          seq)))
  :in-order-to ((test-op (test-op cl-ps-ecs-test))))
