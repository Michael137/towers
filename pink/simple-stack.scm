; Naive stack-machine:
;   (mk-stack) ; ==> ((stack), #t)
;   (push 10) ; ==> ((stack 10), #t)
;   (push 20) ; ==> ((stack 20), #t)
;   (add) ; ==> ((stack 30), #t)
;   (pop) ; ==> ((stack), 30)
;   (pop) ; ==> '(Stack underflow)

(define global-stack '())
(define (mk-stack)
    (begin
        (set! global-stack (list 'stack))
        global-stack))
(define (push num)
    (begin
        (set! global-stack (append global-stack (list num)))
        (cons global-stack #t)))

(define (pop)
    (if (eq? (stack-empty? global-stack) #f)
        (begin
            (set! ret (car (cdr global-stack)))
            (set! global-stack (append (list 'stack) (cdr (cdr global-stack))))
            (cons global-stack ret))
        (display '(Error: stack underflow))))
    

(define (stack-empty? stk)
    (<= (length stk) 1))

; CPS stack machine:
    
; Pink stack machine:
;   Calling convention: evalms( env, exp )
;	    env: program
;	    exp: pink eval expression
