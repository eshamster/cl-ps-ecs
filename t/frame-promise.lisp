(in-package :cl-user)
(defpackage cl-ps-ecs-test.frame-promise
  (:use :cl
        :cl-ps-ecs
        :parenscript
        :ps-experiment
        :prove)
  (:import-from :ps-experiment-test.test-utils
                :with-prove-in-both))
(in-package :cl-ps-ecs-test.frame-promise)

(plan 2)

(declaim #+sbcl (sb-ext:muffle-conditions sb-ext:compiler-note))

(defmacro.ps+ with-modify-env (&body body)
  `(unwind-protect
        (progn ,@body)
     (clean-ecs-env)))

(subtest
    "Test frame-promise-then"
  (with-prove-in-both ()
    (with-modify-env
      (let* ((counter 0)
             (promise1 (init-frame-promise
                        (lambda (resolve)
                          (register-nframes-after-func
                           (lambda () (incf counter) (funcall resolve 10))
                           2))))
             (promise2 (frame-promise-then
                        promise1
                        (lambda (value) (incf counter value) 100))))
        ;; Nothing should be done
        (ecs-main)
        (is counter 0)
        ;; The promise1 should be resolved.
        ;; Then promise2 is also resolved.
        (ecs-main)
        (is counter 11)
        ;; The promise2 should be also frame-promise object
        (frame-promise-then promise2
                            (lambda (value) (incf counter value)))
        (is counter 11)
        (ecs-main)
        (is counter 111))
      ;; Test timeout
      (let ((promise (init-frame-promise
                      (lambda (resolve)
                        (register-nframes-after-func
                         (lambda () (funcall resolve t)) 999)))))
        (frame-promise-then promise
                            (lambda (value) value)
                            :timeout-frame 2)
        (ecs-main)
        (is-error (ecs-main) 'simple-error)))))

(subtest
    "Test frame-promise-all"
  (with-prove-in-both ()
    (with-modify-env
      (let* ((counter 0)
             (promise1 (init-frame-promise
                        (lambda (resolve)
                          (register-nframes-after-func
                           (lambda ()
                             (incf counter 10)
                             (funcall resolve 10))
                           2))))
             (promise2 (init-frame-promise
                        (lambda (resolve)
                          (register-nframes-after-func
                           (lambda ()
                             (incf counter 20)
                             (funcall resolve 20))
                           3))))
             (promise-all (frame-promise-all
                           (list promise1 promise2)
                           (lambda (value-list)
                             (is (length value-list) 2)
                             (is (nth 0 value-list) 10)
                             (is (nth 1 value-list) 20)
                             (setf counter
                                   (* (+ counter (nth 0 value-list))
                                      (nth 1 value-list)))
                             1000))))
        ;; Nothing should be done
        (ecs-main)
        (is counter 0)
        ;; promise1 should be resolved
        (ecs-main)
        (is counter 10)
        ;; The promise2 should be resolved. (counter is 30)
        ;; Then promise-all should be also resolved.
        (ecs-main)
        (is counter 800)
        ;; The promise-all is also frame-promise object
        (frame-promise-then promise-all
                            (lambda (value) (incf counter value)))
        (is counter 800)
        (ecs-main)
        (is counter 1800))
      ;; Test timeout
      (let ((promise (init-frame-promise
                      (lambda (resolve)
                        (register-nframes-after-func
                         (lambda () (funcall resolve t)) 999)))))
        (frame-promise-all (list promise)
                           (lambda (value-list) value-list)
                            :timeout-frame 2)
        (ecs-main)
        (is-error (ecs-main) 'simple-error)))))

(finalize)
