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
(push-k 10 empty-stack (lambda (x) (push-k 20 x disp-k)))
(push-k 10 empty-stack (lambda (x) (push-k 20 x (lambda (y) (pop-k y (lambda (z) (pop-k z (lambda (a) (pop-k a disp-k)))))))))