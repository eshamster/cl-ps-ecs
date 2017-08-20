(in-package :cl-user)
(defpackage cl-ps-ecs.frame-promise
  (:use :cl
        :parenscript
        :ps-experiment
        :cl-ps-ecs.basic-process)
  (:export :frame-promise
           :frame-promise-p
           :init-frame-promise
           :frame-promise-then
           :frame-promise-all))
(in-package :cl-ps-ecs.frame-promise)

;; The frame-promise is inspired from JavaScript's Promise.
;; It is checked that each frame-promise is resolved in first of frame (ecs-main).
;; If it has been resoloved, following frame-promises are invoked.

(defstruct.ps+ frame-promise
    (result nil) (resolved-p nil))

(defun.ps+ frame-promise-resolve (promise result-value)
  (with-slots (result resolved-p) promise
    (setf result result-value
          resolved-p t)))

(defun.ps+ init-frame-promise (callback)
  "Initialize a frame-promise. The callback accepts a function \"resolve\". It should be called after the process is done with its return value."
  (let ((promise (make-frame-promise)))
    (funcall callback
             (lambda (value) (frame-promise-resolve promise value)))
    promise))

(defun.ps+ frame-promise-then (promise callback &key (timeout-frame -1))
  "Register the callback as a following promise of the promise. The callback is invoked after the promise is resolved. Then, it accepts a return value of the promise."
  (let ((promise-new (make-frame-promise)))
    (register-func-with-pred
     (lambda ()
       (frame-promise-resolve
        promise-new
        (funcall callback (frame-promise-result promise))))
     (lambda ()
       (frame-promise-resolved-p promise))
     :timeout-frame timeout-frame)
    promise-new))

(defun.ps+ frame-promise-all (promise-list callback &key (timeout-frame -1))
  "Register the callback as a following promise of the promise list. The callback is invoked after all of the promise are resolved. Then, it accepts a return value list of the promises."
  (let ((promise-new (make-frame-promise)))
    (register-func-with-pred
     (lambda ()
       (frame-promise-resolve
        promise-new
        (funcall callback
                 (mapcar (lambda (promise) (frame-promise-result promise))
                         promise-list))))
     (lambda ()
       (every (lambda (promise) (not (null (frame-promise-resolved-p promise))))
              promise-list))
     :timeout-frame timeout-frame)
    promise-new))
