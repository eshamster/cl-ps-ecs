(defpackage cl-ps-ecs/main
  (:nicknames :cl-ps-ecs)
  (:use :cl-ps-ecs/ecs
        :cl-ps-ecs/basic-process
        :cl-ps-ecs/frame-promise))
(in-package :cl-ps-ecs/main)

(cl-reexport:reexport-from :cl-ps-ecs/ecs)
(cl-reexport:reexport-from :cl-ps-ecs/basic-process
                           :exclude '(:execute-ecs-basic-process
                                      :clean-ecs-basic-process))
(cl-reexport:reexport-from :cl-ps-ecs/frame-promise)
