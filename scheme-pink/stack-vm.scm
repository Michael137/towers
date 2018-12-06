 ; bottom (top?) element of stacks
(define bottom '_/_)
(define nop bottom)

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

(define (nested-idx_ lst elem aux)
    (if (eq? (cadr lst) elem)
        aux
        (nested-idx_ (car lst) elem (+ aux 1))))

(define (index lst elem)
    (nested-idx_ lst elem 0))

(define (repeat-cdr_ lst idx aux)
    (if (eq? idx aux)
        (cdr lst)
        (repeat-cdr_ (car lst) idx (+ aux 1))))

(define (get-by-idx lst idx)
    (repeat-cdr_ lst idx 0))

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
        (set-car! (get-stack stk 'labels) (append (get-stack stk 'labels) (list lbl))) ; improve lookup of labels
        (k stk)))

; make lookup less fragile
(define (jmp-k lbl stk k)
    (k
        ; execute code
        (machine stk
            ; find code i.e. ops using label index
            (list (get-by-idx
                (car (get-stack stk 'code))

                ; find label in labels
                (index (car (get-stack stk 'labels)) lbl)) 'DONE))
        ; resume with updated stack to current continuation
        ))

; Top-level executor
;; stk: stack
;; ops: list of operations i.e. '((PUSH 10) . ((PUSH 20) . ((DONE)))
;;
;; Example:
;;  (machine vm-stack '((PUSH 10) . ((PUSH 20) . ((DONE))))) ==> (20 10 _/_)
;;  (machine vm-stack '((PUSH 10) . ((PUSH 20) . ((POP) . ((DONE)))))) ==> (10 _/_)
;;  (machine vm-stack '((PUSH 10) . ((PUSH 20) . ((POP) . ((POP) . ((POP) . ((DONE)))))))) ==> Underflow
;;  (machine vm-stack '((PUSH 10) . ((PUSH 20) . ((MUL) . ((PUSH #f) . ((LABEL FOO JMP FOO) . ((JMP FOO) . ((DONE))))))))) ==> non-termination
;;  (machine vm-stack '((PUSH 10) . ((PUSH 20) . ((MUL) . ((PUSH #f) . ((LABEL FOO JMP FOO) . ((JMP FOO) . ((DONE))))))))) ==> non-termination
;;  (machine vm-stack '((PUSH 10) . ((PUSH 20) . ((MUL) . ((PUSH #f) . ((LABEL FOO JMP FOO) . ((LABEL BAR JMP FOO) . ((JMP BAR) . ((DONE))))))))) ==> non-termination
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
    (if (eq? 'JMP (caar ops)) (jmp-k (cadr (car ops)) stk (lambda (s) (machine s (cdr ops))))
    (if (eq? nop (caar ops)) (machine stk (remove (lambda (x) (eq? x nop)) ops)) ; nop
    (if (eq? 'DONE (caar ops)) (disp-k stk)
    `(Error: unknown operation ,(caar ops))))))))))))))))))

(define (run ops)
    (begin
        (machine 
            (stack-reset vm-stack id-k) ops)))