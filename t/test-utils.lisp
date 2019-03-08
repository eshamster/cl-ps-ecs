(defpackage cl-ps-ecs/t/test-utils
  (:use :cl
        :parenscript
        :ps-experiment)
  (:export :with-ecs-env)
  (:import-from :cl-ps-ecs/ecs
                :clean-ecs-env))
(in-package :cl-ps-ecs/t/test-utils)

;; TOOD: The with-ecs-env simply clean environment after test.
;;       But we should restore environment before test.

(defmacro.ps+ with-ecs-env (() &body body)
  "Execute body and clean ECS environment after that."
  `(unwind-protect
        (progn ,@body)
     (clean-ecs-env)))
