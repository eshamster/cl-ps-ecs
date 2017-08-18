#|
  This file is a part of cl-ps-ecs project.
  Copyright (c) 2015 eshamster (hamgoostar@gmail.com)
|#

(in-package :cl-user)
(defpackage cl-ps-ecs-test-asd
  (:use :cl :asdf))
(in-package :cl-ps-ecs-test-asd)

(defsystem cl-ps-ecs-test
  :author "eshamster"
  :license "LLGPL"
  :depends-on (:cl-ps-ecs
               :parenscript
               :ps-experiment
               :ps-experiment-test
               :cl-js
               :prove)
  :components ((:module "t"
                :components
                ((:test-file "utils")
                 (:test-file "ecs")
                 (:test-file "frame-promise")
                 (:test-file "cl-ps-ecs"))))
  :description "Test system for cl-ps-ecs"

  :defsystem-depends-on (:prove-asdf)
  :perform (test-op :after (op c)
                    (funcall (intern #.(string :run-test-system) :prove-asdf) c)
                    (asdf:clear-system c)))
