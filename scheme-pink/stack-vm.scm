 ; bottom (top?) element of stacks
(define bottom '_/_)

; Memory (TODO: currently operates just on global)
(define static-stack (list bottom))
(define local-stack (list bottom))
(define arg-stack (list bottom))
(define cst-stack (list bottom))
(define global-stack (list bottom))

; CPS helpers
(define (disp-k x) (display x))
(define (id-k x) x)

; Stack helpers
(define (stack-empty-k? stk)
    (eq? (cdr stk) '()))

(define (get-stack label)
    (if (eq? label 'static) static-stack
    (if (eq? label 'local) local-stack
    (if (eq? label 'arg) arg-stack
    (if (eq? label 'cst) cst-stack    
    (if (eq? label 'global) global-stack        
    '(Error: wrong stack label)))))))

; Stack operations
(define (push-k num stk k)
    (k (cons stk num)))

(define (pop-k stk k)
    (k (if (not (stack-empty-k? stk))
        (cdr stk)
        '(Error: stack underflow))))

(define (add-k stk k)
    (k (cons (+ (car stk) (cadr stk)) (cddr stk))))

; Top-level executor
;; op: operation i.e. '(PUSH 10)
;; stk: stack
;;
;; Example:
;;  (machine global-stack '((PUSH 10) . ((PUSH 20) . ((DONE))))) ==> (20 10 _/_)
;;  (machine global-stack '((PUSH 10) . ((PUSH 20) . ((POP) . ((DONE)))))) ==> (10 _/_)
;;  (machine global-stack '((PUSH 10) . ((PUSH 20) . ((POP) . ((POP) . ((POP) . ((DONE)))))))) ==> Underflow
(define (machine stk ops)
    (if (eq? 'PUSH (caar ops)) (push-k stk (car (cdr (car ops))) (lambda (s) (machine s (cdr ops))))
    (if (eq? 'POP (caar ops)) (pop-k stk (lambda (s) (machine s (cdr ops))))
    (if (eq? 'ADD (caar ops)) (add-k stk (lambda (s) (machine s (cdr ops))))
    (if (eq? 'DONE (caar ops)) (disp-k stk)
    `(Error: unknown operation ,(caar ops)))))))