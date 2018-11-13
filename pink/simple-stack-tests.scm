(load "simple-stack.scm")

(define (run-test e)
    (begin
        (display (format "~s " e))
        (display "===> ")
        (display (eval e))
        (display (format "~%"))))

(run-test '(mk-stack))
(run-test '(push 10))
(run-test '(push 40))
(run-test '(push 45))
(run-test '(pop))
(run-test '(pop))
(run-test '(pop))
(run-test '(pop))