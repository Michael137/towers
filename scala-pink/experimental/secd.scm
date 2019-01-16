(define vm-stack '())
(define env-list '())
(define op-list '())
(define call-stack '())
(define loc (lambda (y lst) (if (eq? y 1) (car lst) (loc (- y 1) (cdr lst)))))
(define locate (lambda (i j env) (loc j (loc i env))))
(define machine (lambda (s e c d ops)
                    (if (eq? 'STOP (car ops)) s
                    (if (eq? 'LDC (car ops)) (machine (cons (cadr ops) s) e c d (cddr ops))
                    (if (eq? 'LD (car ops))
                        (machine (cons (locate (car (cadr ops)) (cadr (cadr ops)) e) s) e c d (cddr ops))
                    (if (eq? 'ADD (car ops)) (machine (cons (+ (car s) (cadr s)) (cddr s)) e c d (cdr ops))
                    (if (eq? 'SUB (car ops)) (machine (cons (- (car s) (cadr s)) (cddr s)) e c d (cdr ops))
                    (if (eq? 'MPY (car ops)) (machine (cons (* (car s) (cadr s)) (cddr s)) e c d (cdr ops))
                    (if (eq? 'EQ (car ops)) (machine (cons (eq? (car s) (cadr s)) (cddr s)) e c d (cdr ops))
                    (if (eq? 'GT (car ops)) (machine (cons (> (car s) (cadr ops)) (cdr s)) e c d (cddr ops))
                    (if (eq? 'CONS (car ops)) (machine (cons (cons (car s) (cadr s)) (cddr s)) e c d (cdr ops))
                    (if (eq? 'NIL (car ops)) (machine (cons '() s) e c d (cdr ops))
                    (if (eq? 'SEL (car ops))
                        (if (car s)
                            (machine (cdr s) e c (cons (cdddr ops) d) (cadr ops))
                            (machine (cdr s) e c (cons (cdddr ops) d) (caddr ops)))
                    (if (eq? 'DUM (car ops)) (machine s (cons '() e) c d (cdr ops))
                    (if (eq? 'JOIN (car ops))
                        (machine s e c (cdr d) (car d))

                    (if (eq? 'LDF (car ops)) (machine (cons (cons (cadr ops) e) s) e c d (cddr ops)) ; (machine (cons (cons (cadr ops) e) s) e c d (cdr ops))
                    (if (eq? 'AP (car ops))
                        (machine '() (cons (cadr s) (cdr (car s))) c (cons (cddr s) (cons e (cons (cdr ops) d))) (caar s))
                    (if (eq? 'RTN (car ops))
                        (let ((resume (caddr d)))
                            (machine (cons (car s) (car d)) (cadr d) c (cdddr d) resume))
                    (if (eq? 'RAP (car ops))
                        (begin
                            (set-car! e (cadr s))
                            (machine '() e c (cons (cddr s) (cons e (cons (cdr ops) d))) (caar s)))

                            ; (
                            ;     (
                            ;         (NIL LD (2 2) CONS LD (2 1) CONS LD (1 1) AP RTN)
                            ;         ()
                            ;         (10 1)
                            ;     )
                            ;     (
                            ;         (
                            ;             (LDC 0 LD (1 1) EQ SEL (LDC 1 JOIN) (NIL LD (1 2) LD (1 1) MPY CONS LD (3 2) LD (1 1) SUB CONS LD (2 1) AP JOIN) RTN)
                            ;             ()
                            ;             (10 1)
                            ;         )
                            ;     )
                            ; )

                    (if (eq? 'DBG (car ops)) s;(begin (display (cadr s))
                                              ;       (machine s e c d (cdr ops)))
                                            
                    (if (eq? 'WRITEC (car ops)) (car e)
                        (machine s e c d (cdr ops)))))))))))))))))))))))

(define (start ops) (machine vm-stack env-list op-list call-stack ops))

(define fac_src '(NIL LDC 1 CONS LDC 10 CONS LDF
                        (DUM NIL LDF
                            (LDC 0 LD (1 1) EQ SEL
                                (LDC 1 JOIN) ; (LD (1 2) JOIN)
                                (NIL LD (1 2) LD (1 1) MPY CONS
                                        LD (3 2) LD (1 1) SUB CONS LD (2 1) AP JOIN)
                            RTN)
                        CONS LDF
                            (NIL LD (2 2) CONS LD (2 1) CONS LD (1 1) AP RTN) DBG RAP
                        RTN) AP STOP))

(define test_src '(NIL LDC 1 CONS LDC 2 CONS LDF  
                    (NIL LDC 3 CONS LDC 4 CONS 
                    LDF 
                        (NIL LDC 5 CONS LDC 6 CONS 
                        LDF (LD (3 1) LD (2 2) LD (1 1) SUB ADD RTN) AP RTN)
                    AP 
                    RTN) 
                    AP STOP))

(display (start fac_src))
(display (start test_src))