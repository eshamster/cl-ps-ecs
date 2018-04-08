#|
  This file is a part of cl-ps-ecs project.
  Copyright (c) 2015 eshamster (hamgoostar@gmail.com)
|#

#|
  Author: eshamster (hamgoostar@gmail.com)
|#

(defsystem cl-ps-ecs
  :version "0.1"
  :author "eshamster"
  :license "LLGPL"
  :class :package-inferred-system
  :depends-on (:parenscript
               :alexandria
               :ps-experiment
               :cl-reexport
               "cl-ps-ecs/main")
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
  :in-order-to ((test-op (test-op cl-ps-ecs/t))))

(defsystem cl-ps-ecs/t
  :class :package-inferred-system
  :depends-on (:ps-experiment
               :ps-experiment/t/test-utils
               :parenscript
               :cl-js
               :rove
               "cl-ps-ecs/t/utils"
               "cl-ps-ecs/t/flat-tree"
               "cl-ps-ecs/t/basic-process"
               "cl-ps-ecs/t/frame-promise"
               "cl-ps-ecs/t/ecs")
  :perform (test-op (o c) (symbol-call :rove '#:run c)))
