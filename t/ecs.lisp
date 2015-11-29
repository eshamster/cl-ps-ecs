(in-package :cl-user)
(defpackage cl-ps-ecs-test.ecs
  (:use :cl
        :cl-ps-ecs 
        :prove)
  (:import-from :ps-experiment-test.test-utils
                :prove-in-both))
(in-package :cl-ps-ecs-test.ecs)

(plan nil)

(prove-in-both (is (+ 1 2) 3))

(finalize)
