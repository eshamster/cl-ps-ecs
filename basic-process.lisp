(defpackage cl-ps-ecs/basic-process
  (:use :cl
        :parenscript
        :ps-experiment)
  (:export :execute-ecs-basic-process
           :clean-ecs-basic-process

           :register-next-frame-func
           :register-nframes-after-func
           :register-func-with-pred))
(in-package :cl-ps-ecs/basic-process)

(defvar.ps+ *next-frame-func-list* '())

(defun.ps+ register-next-frame-func (func)
  "Register a function with no argument that is executed in first of next frame.
Note: Some functions (Ex. add or delete resource) can cause troublesome problems if it is executed in frame. Use this wrapper when executing such functions."
  (push func *next-frame-func-list*))

(defstruct.ps+ func-with-pred func pred rest-timeout-frame name)

(defvar.ps+ *func-with-pred-list* '())

(defun.ps+ execute-all-registered-funcs-with-pred ()
  (let ((executed-list '()))
    (dolist (func-with-pred (reverse *func-with-pred-list*))
      (with-slots (func pred rest-timeout-frame name) func-with-pred
        (if (funcall pred)
            (progn (funcall func)
                   (push func-with-pred executed-list))
            (when (> rest-timeout-frame 0)
              (decf rest-timeout-frame)
              (when (= rest-timeout-frame 0)
                (error "The function with predication \"~A\" ends with timeout" name))))))
    (dolist (executed executed-list)
      (setf *func-with-pred-list*
            (remove executed *func-with-pred-list*)))))

(defun.ps+ register-func-with-pred (func pred &key (timeout-frame -1) (name ""))
  "Register a function that will be executed when the predication function return true in first of a frame.
The name is not used in the process but it is useful for debug."
  (push (make-func-with-pred :func func :pred pred
                             :rest-timeout-frame timeout-frame
                             :name name)
        *func-with-pred-list*))

(defun.ps+ register-nframes-after-func (func delayed-frames)
  "Register a function with no argument that is executed N frames after.
Ex. If delayed-frames is 1, it will be executed in its next frame. If 2, executed in its next after next frame."
  (let ((rest-time delayed-frames))
    (register-func-with-pred func
                             (lambda ()
                               (decf rest-time)
                               (<= rest-time 0)))))

(defun.ps+ execute-all-registered-funcs ()
  ;; Reverse to execute functions in order of registration.
  (let ((func-list (reverse *next-frame-func-list*)))
    (setf *next-frame-func-list* '())
    (dolist (func func-list)
      (funcall func))))

(defun.ps+ execute-ecs-basic-process ()
  (execute-all-registered-funcs)
  (execute-all-registered-funcs-with-pred))

(defun.ps+ clean-ecs-basic-process ()
  (setf *next-frame-func-list* '())
  (setf *func-with-pred-list* '()))
