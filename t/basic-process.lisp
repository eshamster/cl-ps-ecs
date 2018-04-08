(defpackage cl-ps-ecs/t/basic-process
  (:use :cl
        :cl-ps-ecs/basic-process
        :parenscript
        :ps-experiment
        :rove
        :ps-experiment/t/test-utils))
(in-package :cl-ps-ecs/t/basic-process)

(declaim #+sbcl (sb-ext:muffle-conditions sb-ext:compiler-note))

(defmacro.ps+ with-modify-env (&body body)
  `(unwind-protect
        (progn ,@body)
     (clean-ecs-basic-process)))

(deftest.ps+ for-register-next-frame-func
  (with-modify-env
    (let ((counter 0))
      ;; Check if 2 functions are executed in order of registration
      (register-next-frame-func #'(lambda () (incf counter)))
      (register-next-frame-func #'(lambda () (setf counter (* 2 counter))))
      (ok (= counter 0))
      (incf counter 10)
      (ok (= counter 10))
      (execute-ecs-basic-process)
      (ok (= counter 22))
      ;; Check if registered functions are cleared after execution
      (execute-ecs-basic-process)
      (ok (= counter 22)))))

(deftest.ps+ for-register-nframes-after-func
  (with-modify-env
    (let ((counter 0))
      (flet ((make-adder (n)
               (lambda () (incf counter n)))
             (make-pred (border)
               (lambda () (>= counter border))))
        ;; func1-1, 1-2
        (register-nframes-after-func (make-adder 1) 0)
        (register-nframes-after-func (make-adder 10) 1)
        ;; func2
        (register-nframes-after-func (make-adder 100) 3)
        (ok (= counter 0))
        ;; only func1-1 and 1-2 should be invoked (and removed)
        (execute-ecs-basic-process)
        (ok (= counter 11))
        ;; nothing should be invoked
        (execute-ecs-basic-process)
        (ok (= counter 11))
        ;; only func2 should be invokded
        (execute-ecs-basic-process)
        (ok (= counter 111))))))

(deftest.ps+ for-register-func-with-pred
  (with-modify-env
    (let ((counter 0))
      (flet ((make-adder (n)
               (lambda () (incf counter n)))
             (make-pred (border)
               (lambda () (>= counter border))))
        ;; func1
        (register-func-with-pred (make-adder 100) (make-pred 10))
        ;; func2
        (register-func-with-pred (make-adder 10) (make-pred -1))
        ;; func3
        (register-func-with-pred (make-adder 1000) (make-pred 999999)
                                 :timeout-frame 3)
        (ok (= counter 0))
        ;; only func2 should be invoked (and removed)
        (execute-ecs-basic-process)
        (ok (= counter 10))
        ;; only func1 should be invokded
        (execute-ecs-basic-process)
        (ok (= counter 110))
        ;; should be timeout error because of func3
        (ok (signals (execute-ecs-basic-process) 'simple-error))))))
