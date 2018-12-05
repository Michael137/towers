 ; bottom (top?) element of stacks
(define bottom '_/_)

; Memory
(define global-stack (list bottom))
(define code (list bottom))
(define labels (list bottom))
(define vm-stack (list code labels global-stack))

; CPS helpers
(define (disp-k x) (display x))
(define (id-k x) x)

; Stack helpers
(define (stack-empty-k? stk)
    (eq? (cdr stk) '()))

(define (get-stack vm-stk label)
    (if (eq? label 'code) (car vm-stack)
    (if (eq? label 'labels) (cadr vm-stack)
    (if (eq? label 'global) (caddr vm-stack)
    '(Error: wrong stack label)))))

(define (binop-k op stk k)
    (k (cons (op (car stk) (cadr stk)) (cddr stk))))

(define (binop-k-rev op stk k)
    (k (cons (op (cadr stk) (car stk)) (cddr stk))))

(define (stack-reset stk k)
    (begin
        (map (lambda (x) (set-car! x bottom) stk)
        stk)))

; Stack operations
(define (push-k num stk k)
    (k (cons stk num)))

(define (pop-k stk k)
    (k (if (not (stack-empty-k? stk))
        (cdr stk)
        '(Error: stack underflow))))

(define (add-k stk k)
    (binop-k + stk k))

(define (sub-k stk k)
    (binop-k-rev - stk k))

(define (mul-k stk k)
    (binop-k - stk k))

(define (neg-k stk k)
    (k (cons (* (car stk) -1) (cdr stk))))

(define (lt-k stk k)
    (binop-k < stk k))

(define (gt-k stk k)
    (binop-k > stk k))

(define (and-k stk k)
    (k (cons (and (car stk) (cadr stk)) (cddr stk))))

(define (or-k stk k)
    (k (cons (or (car stk) (cadr stk)) (cddr stk))))

(define (not-k stk k)
    (k (cons (not (car stk)) (cdr stk))))

(define (save-label-k lbl code stk k)
    (begin
        (set-car! (get-stack stk 'code) (append (get-stack stk 'code) code))
        (set-car! (get-stack stk 'labels) (append (get-stack stk 'labels) lbl)) ; improve lookup of labels
        (k stk)))

; call

; if

; Top-level executor
;; stk: stack
;; ops: list of operations i.e. '((PUSH 10) . ((PUSH 20) . ((DONE)))
;;
;; Example:
;;  (machine vm-stack '((PUSH 10) . ((PUSH 20) . ((DONE))))) ==> (20 10 _/_)
;;  (machine vm-stack '((PUSH 10) . ((PUSH 20) . ((POP) . ((DONE)))))) ==> (10 _/_)
;;  (machine vm-stack '((PUSH 10) . ((PUSH 20) . ((POP) . ((POP) . ((POP) . ((DONE)))))))) ==> Underflow
;;  (machine vm-stack '((PUSH 10) . ((PUSH 20) . ((MUL) . ((PUSH #f) . ((LABEL FOO JMP FOO) . ((JMP FOO) . ((DONE))))))))) ==> non-termination
(define (machine stk ops)
    (if (eq? 'PUSH (caar ops)) (push-k stk (car (cdr (car ops))) (lambda (s) (machine s (cdr ops))))
    (if (eq? 'POP (caar ops)) (pop-k stk (lambda (s) (machine s (cdr ops))))
    (if (eq? 'ADD (caar ops)) (add-k stk (lambda (s) (machine s (cdr ops))))
    (if (eq? 'SUB (caar ops)) (sub-k stk (lambda (s) (machine s (cdr ops))))
    (if (eq? 'MUL (caar ops)) (mul-k stk (lambda (s) (machine s (cdr ops))))
    (if (eq? 'NEG (caar ops)) (neg-k stk (lambda (s) (machine s (cdr ops))))
    (if (eq? 'LT (caar ops)) (lt-k stk (lambda (s) (machine s (cdr ops))))
    (if (eq? 'GT (caar ops)) (gt-k stk (lambda (s) (machine s (cdr ops))))
    (if (eq? 'AND (caar ops)) (and-k stk (lambda (s) (machine s (cdr ops))))
    (if (eq? 'OR (caar ops)) (or-k stk (lambda (s) (machine s (cdr ops))))
    (if (eq? 'NOT (caar ops)) (not-k stk (lambda (s) (machine s (cdr ops))))
    (if (eq? 'LABEL (caar ops)) (save-label-k (cadr (car ops)) (cddr (car ops)) stk (lambda (s) (machine s (cdr ops))))
    (if (eq? 'JMP (caar ops)) (jmp-k (cadr (car ops)) (lambda (s) (machine s (cdr ops))))
    (if (eq? 'DONE (caar ops)) (disp-k stk)
    `(Error: unknown operation ,(caar ops)))))))))))))))))

; if n == 0 1 else n * factorial (n-1)
; <=>
; PUSH 10
; LABEL fac
;   POPS arg local
;   PUSH
;   MUL
;   POPS local arg
;   POP local ; move multiplied value from stack to local
;
;   DEC arg
;   JMP fac
;   
;
; loop:
; 	; If the counter is less or equal to 1, finish
; 	cmp ecx, 1
; 	jle end
; 	
; 	sub ecx, 1
; 	mul ecx
; 	jmp loop