(define (run-test e)
    (begin
        (display (format "~s " e))
        (display "===> ")
        (display (eval e))
        (display (format "~%"))))

(define (simple-vm-tests)
    (run-test '(mk-stack))
    (run-test '(push 10))
    (run-test '(push 40))
    (run-test '(push 45))
    (run-test '(pop))
    (run-test '(pop))
    (run-test '(pop))
    (run-test '(pop))
    (push-k 10 empty-stack (lambda (x) (push-k 20 x disp-k)))
    (push-k 10 empty-stack (lambda (x) (push-k 20 x (lambda (y) (pop-k y (lambda (z) (pop-k z (lambda (a) (pop-k a disp-k))))))))))

(define (stack-vm-tests)
    (run-test '(factorial 1000))
    (run-test '(run '((PUSH 10) .
                     ((PUSH 20) .
                     ((MUL) .
                     ((PUSH #f) .
                     ((LABEL FOO JMP FOO) .
                     ((LABEL BAR ((PUSH 10) .
                                 ((RET)))) .
                     ((LABEL BAZ JMP FOO) .
                     ((JMP BAR) .
                     ((JMP BAR) .
                     ((JMP BAR) .
                     ((JMP BAR) .
                     ((PUSH 20) .
                     ((OR) .
                     ((RET))))))))))))))))))

(define (run-tests)
    (begin
        (load "simple-stack.scm")
        (simple-vm-tests)

        (load "stack-vm.scm")
        (stack-vm-tests)))