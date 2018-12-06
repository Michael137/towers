 ; bottom (top?) element of stacks
(define bottom '_/_)
(define nop bottom)

; Memory
(define locals (list bottom))
(define code (list bottom))
(define labels (list bottom))
(define vm-stack (list code labels locals))

; CPS helpers
(define (disp-k x) (display x))
(define (id-k x) x)

; Stack helpers
(define (stack-empty-k? stk)
    (eq? (cdr stk) '()))

(define (get-stack vm-stk label)
    (if (eq? label 'code) (car vm-stack)
    (if (eq? label 'labels) (cadr vm-stack)
    (if (eq? label 'locals) (caddr vm-stack)
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

(define (jmp-k lbl stk k)
    (k
        ; execute code
        (machine stk
            ; find code i.e. ops using label index
            (car (append (get-by-idx
                (car (get-stack stk 'code))

                ; find label in labels
                (index (car (get-stack stk 'labels)) lbl)) '(RET))))
        ; resume with updated stack to current continuation
        ))

; TODO: reuse normal stack operations instead of creating new ones
(define (local-k stk ops k)
    (k
        (if (eq? (car ops) 'POP)
            (begin
                (set-car! (get-stack stk 'locals) (append (get-stack stk 'locals) (car stk))) ; push new value onto locals stack
                (cdr stk)) ; Pop top element of global stack
        (if (eq? (car ops) 'DUP)
            (begin
                (set-car! (get-stack stk 'locals) (append (get-stack stk 'locals) (cdr (car (get-stack stk 'locals)))))
                (cdr stk))
        (if (eq? (car ops) 'SUB)
            (begin
                (set-car! (get-stack stk 'locals) (append (get-stack stk 'locals) (- (cdr (caar (get-stack stk 'locals))) ; TODO: pop elements properly
                                                                                     (cdr (car (get-stack stk 'locals))))))
                (cdr stk))
        (if (eq? (car ops) 'MUL)
            (begin
                (set-car! (get-stack stk 'locals) (append (get-stack stk 'locals) (* (cdr (car (get-stack stk 'locals))) ; TODO: pop elements properly
                                                                                     (cdr (caar (get-stack stk 'locals))))))
        `(Error: operation ,(car ops) on local segment not supported))))))))

; Top-level executor
;; stk: stack
;; ops: list of operations i.e. '((PUSH 10) . ((PUSH 20) . ((PRINT)))
;;
;; Example:
;;  (machine vm-stack '((PUSH 10) . ((PUSH 20) . ((PRINT))))) ==> (20 10 _/_)
;;  (machine vm-stack '((PUSH 10) . ((PUSH 20) . ((POP) . ((PRINT)))))) ==> (10 _/_)
;;  (machine vm-stack '((PUSH 10) . ((PUSH 20) . ((POP) . ((POP) . ((POP) . ((PRINT)))))))) ==> Underflow
;;  (machine vm-stack '((PUSH 10) . ((PUSH 20) . ((MUL) . ((PUSH #f) . ((LABEL FOO JMP FOO) . ((JMP FOO) . ((PRINT))))))))) ==> non-termination
;;  (machine vm-stack '((PUSH 10) . ((PUSH 20) . ((MUL) . ((PUSH #f) . ((LABEL FOO JMP FOO) . ((JMP FOO) . ((PRINT))))))))) ==> non-termination
;;  (machine vm-stack '((PUSH 10) . ((PUSH 20) . ((MUL) . ((PUSH #f) . ((LABEL FOO JMP FOO) . ((LABEL BAR JMP FOO) . ((JMP BAR) . ((PRINT)))))))))) ==> non-termination
;;
;;  (run '((PUSH 10) . ((PUSH 20) . ((MUL) . ((PUSH #f) . ((LABEL FOO JMP FOO) . ((LABEL BAR ((PUSH 10) . RET)) . ((LABEL BAZ JMP FOO) . ((JMP BAR) . ((JMP BAZ) . ((PRINT)))))))))))) ==> non-termination
;;  (run '((PUSH 10) . ((PUSH 20) . ((MUL) . ((PUSH #f) . ((LABEL FOO JMP FOO) . ((LABEL BAR ((PUSH 10) . RET)) . ((LABEL BAZ JMP FOO) . ((JMP BAR) . ((JMP BAR) . ((PUSH 20) .
;;      ((OR) . ((SEGMENT code)))))))))))))) ==> all code associated with labels
;;
;;  (run '((PUSH 10) . ((PUSH 20) . ((MUL) . ((PUSH #f) . ((LABEL FOO JMP FOO) . ((LABEL BAR ((PUSH 10) . RET)) . ((LABEL BAZ JMP FOO) . ((JMP BAR) . ((JMP BAR) . ((PUSH 20) .
;;      ((OR) . ((JE 10 FOO) . ((PRINT))))))))))))))) ==> non-termination
(define (machine stk ops)
    ; Primitives
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
    (if (eq? 'JE (caar ops)) ; Jump if equal (pops top element)
        (if (eq? (cadr (car ops)) (car stk))
            (jmp-k (caddr (car ops)) (cdr stk) (lambda (s) (machine s (cdr ops))))
            (machine (cdr stk) (cdr ops)))
    (if (eq? 'DUP (caar ops)) (push-k stk (car stk) (lambda (s) (machine s (cdr ops)))) ; Duplicate top element of stack
    ; TODO: revise memory segmenting
    (if (eq? 'LOCAL (caar ops)) (local-k stk (cdr (car ops)) (lambda (s) (machine s (cdr ops))))

    ; Non-terminating sentinels
    (if (eq? nop (caar ops)) (machine stk (remove (lambda (x) (eq? x nop)) ops)) ; nop

    ; Terminating sentinels
    (if (eq? 'PRINT (caar ops)) (disp-k (filter (lambda (x) (not (list? x))) stk))
    (if (eq? 'SEGMENT (caar ops)) (disp-k (get-stack stk (cadr (car ops))))
    (if (eq? 'RET (caar ops)) stk
    (begin
    (display ops)
    `(Error: unknown operation ,(caar ops))))))))))))))))))))))))

(define (run ops)
    (begin
        (machine 
            (stack-reset vm-stack id-k) ops)))

(define (factorial n)
    (run `(
                (PUSH ,n) .
                ((LABEL fac 
                    ((DUP) .
                    ((LOCAL POP) .
                    ((LOCAL DUP) .
                    ((PUSH 1) .
                    ((LOCAL POP) .
                    ((LOCAL SUB) .
                    ((LOCAL MUL) .
                    ((PUSH 1) .
                    ((SUB) .
                    ((GT 1) .
                    ((JE #t fac))))))))))))) .
                ((JMP fac) .
                ((PRINT)))
            ))))

; if n < 1 then 1 else n * fac(n - 1)
;
; <=>
;
; ; n = 10
; PUSH 10
; LABEL fac:
;
;     ; n * n - 1
;     DUP
;     LOCAL POP
;     PUSH 1
;     LOCAL POP
;     LOCAL SUB
;     LOCAL MUL
;
;     ; n += 1
;     PUSH 1
;     SUB
;
;     ; if n <= 1 then return
;     GT 1
;     JE #t fac
;
; ; fac(n)
; JMP fac