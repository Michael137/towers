// VM purposefully designed for tower PE
// Tests that demonstrate PE
// Cross-level persistence

import EBase._
import ELisp._
import EPink._
import TestHelpers._

object PE {
    val src = """
            (let dbgCtr 10
                (letrec ((locate (lambda (i j env)
                            (letrec ((loc (lambda (y lst) (if (eq? y 1) (car_ lst) (loc (- y 1) (cdr_ lst))))))
                                    (loc j (loc i env))))
                        )
                        (machine (lambda (s d ops e)
                                                (if (eq? 'STOP (car_ ops))
                                                    s
                                                    (let m (lambda (env)
                                                                (if (eq? 'WRITEC (car_ ops))
                                                                    (ref (car_ s))
                                                                (if (eq? 'DBG (car_ ops))
                                                                    (let toLog (let e s `(stack: ,(listref e) car: ,(car_ e) cdr: ,(cdr_ e) car->caddr: ,(caddr_ (car_ e))))
                                                                        (if (eq? dbgCtr 0)
                                                                            toLog
                                                                            (let _ (log 0 toLog) (let _ (set! dbgCtr (- dbgCtr 1)) ((machine s d (cdr_ ops)) env)))))
                                                                (if (eq? 'DBG2 (car_ ops))
                                                                    (car_ (car_ s))
                                                                (if (eq? 'LDC (car_ ops))
                                                                    (machine (cons_ (maybe-lift (cadr_ ops)) s) d (cddr_ ops) env)
                                                                (if (eq? 'ADD (car_ ops))
                                                                    (machine (cons_ (+ (car_ s) (cadr_ s)) (cddr_ s)) d (cdr_ ops) env)
                                                                (if (eq? 'SUB (car_ ops))
                                                                    (machine (cons_ (- (car_ s) (cadr_ s)) (cddr_ s)) d (cdr_ ops) env)
                                                                (if (eq?  'LD (car_ ops))
                                                                    (machine (cons_ (locate (car_ (cadr_ ops)) (cadr_ (cadr_ ops)) env) s) d (cddr_ ops) env)
                                                                (if (eq? 'LDF (car_ ops)) 
                                                                    (machine (cons_ (cons_ (cadr_ ops) env) s) d (cddr_ ops) env)
                                                                (if (eq?  'NIL (car_ ops))
                                                                    (machine (cons_ (maybe-lift '()) s) d (cdr_ ops) env)
                                                                (if (eq? 'AP (car_ ops))
                                                                    (machine '() (cons_ (cddr_ s) (cons_ env (cons_ (cdr_ ops) d))) (ref (caar_ s)) (cons_ (cadr_ s) (cdr_ (car_ s))))
                                                                (if (eq? 'RTN (car_ ops))
                                                                    (let resume (ref (caddr_ d))
                                                                        (machine (cons_ (car_ s) (car_ d)) (cdddr_ d) resume (cadr_ d)))
                                                                (if (eq?  'CONS (car_ ops)) (machine (cons_ (cons_ (car_ s) (cadr_ s)) (cddr_ s)) d (cdr_ ops) env)

                                                                (if (eq? 'SEL (car_ ops))
                                                                    (if (eq? (ref (car_ s)) (maybe-lift 0))
                                                                        (machine (cdr_ s) (cons_ (cdddr_ ops) d) (caddr_ ops) env)
                                                                        (machine (cdr_ s) (cons_ (cdddr_ ops) d) (cadr_ ops) env))
                                                                (if (eq? 'JOIN (car_ ops))
                                                                        (machine s (cdr_ d) (ref (car_ d)) env)
                                                                (if (eq? 'MPY (car_ ops))
                                                                    (machine (cons_ (* (car_ s) (cadr_ s)) (cddr_ s)) d (cdr_ ops) env)
                                                                (if (eq? 'EQ (car_ ops))
                                                                    (machine (cons_ (eq? (ref (car_ s)) (ref (cadr_ s))) (cddr_ s)) d (cdr_ ops) env)
                                                                (if (eq? 'DUM (car_ ops))
                                                                    (machine s d (cdr_ ops) (cons_ '() env))
                                                                (if (eq? 'RAP (car_ ops))
                                                                    (let _ (set-car!_ env (cadr_ s))
                                                                        (machine '() (cons_ (cddr_ s) (cons_ env (cons_ (cdr_ ops) d))) (ref (caar_ s)) env))

                                                                (if (eq? 'CAR (car_ ops)) (machine (cons_ (car_ (car_ s)) (cdr_ s)) d (cdr_ ops) env)
                                                                (if (eq? 'CDR (car_ ops)) (machine (cons_ (cdr_ (car_ s)) (cdr_ s)) d (cdr_ ops) env)
                                                                (if (eq? 'QUOTE (car_ ops)) (machine (cons_ `(,(car_ s)) (cdr_ s)) d (cdr_ ops) env)

                                                                (if (eq? 'CADR (car_ ops)) (machine (cons_ (cadr_ (car_ s)) (cdr_ s)) d (cdr_ ops) env)
                                                                (if (eq? 'CADDR (car_ ops)) (machine (cons_ (caddr_ (car_ s)) (cdr_ s)) d (cdr_ ops) env)
                                                                (if (eq? 'CADDDR (car_ ops)) (machine (cons_ (cadddr_ (car_ s)) (cdr_ s)) d (cdr_ ops) env)

                                                                (if (eq? 'EMPTY? (car_ ops))
                                                                    (machine (cons_ (null? (car_ s)) (cdr_ s)) d (cdr_ ops) env)

                                                                (if (eq? 'ATOM? (car ops))
                                                                    (let a (car_ s)
                                                                        (let isAtom (if (sym? a) (maybe-lift 1) (if (num? a) (maybe-lift 1) (maybe-lift 0)))
                                                                            (machine (cons_ isAtom (cdr_ s)) d (cdr_ ops) env)))

                                                                (maybe-lift 'ERROR))))))))))))))))))))))))))))
                                                        (m e)))
                                        )
                        ))

                        (let match (lambda (ops)
                                        (if (eq?  'STOP (car_ ops))
                                            (maybe-lift (lambda (s) (maybe-lift '(Nothing to run))))
                                            (maybe-lift (machine '() '() ops)))
                                    )
                            match)
                    ))
        """

        val src_curried = """
            (let dbgCtr 10
                (letrec ((locate (lambda (i) (lambda (j) (lambda (env)
                            (letrec ((loc (lambda (y) (lambda (lst) (if (eq? y 1) (car_ lst) ((loc (- y 1)) (cdr_ lst)))))))
                                    ((loc j) ((loc i) env))))))
                        )
                        (machine (lambda (s) (lambda (d) (lambda (ops) (lambda (e)
                                                (if (eq? 'STOP (car_ ops))
                                                    (maybe-lift s)
                                                (if (eq? 'WRITEC (car_ ops))
                                                    (ref (car_ s))
                                                (if (eq? 'DBG (car_ ops))
                                                    (let toLog s
                                                        (if (eq? dbgCtr 0)
                                                            toLog
                                                            (let _ (log 0 toLog) (let _ (set! dbgCtr (- dbgCtr 1)) ((((machine s) d) (cdr_ ops)) e)))))

                                                (if (eq? 'LDC (car_ ops))
                                                    ((((machine (cons_ (maybe-lift (cadr_ ops)) s)) d) (cddr_ ops)) e)
                                                (if (eq? 'ADD (car_ ops))
                                                    ((((machine (cons_ (+ (car_ s) (cadr_ s)) (cddr_ s))) d) (cdr_ ops)) e)
                                                (if (eq? 'SUB (car_ ops))
                                                    ((((machine (cons_ (- (car_ s) (cadr_ s)) (cddr_ s))) d) (cdr_ ops)) e)
                                                (if (eq?  'LD (car_ ops))
                                                    ((((machine (cons_ (((locate (car_ (cadr_ ops))) (cadr_ (cadr_ ops))) e) s)) d) (cddr_ ops)) e)
                                                (if (eq? 'LDF (car_ ops)) 
                                                    ((((machine (cons_ (cons_ (cadr_ ops) e) s)) d) (cddr_ ops)) e)
                                                (if (eq?  'NIL (car_ ops))
                                                    ((((machine (cons_ (maybe-lift '()) s)) d) (cdr_ ops)) e)
                                                (if (eq? 'AP (car_ ops))
                                                    (let oldS s
                                                        ((((machine (maybe-lift '())) (cons_ (cddr_ oldS) (cons_ e (cons_ (cdr_ ops) d)))) (ref (caar_ oldS))) (cons_ (cadr_ oldS) (cdr_ (car_ oldS)))))
                                                (if (eq? 'RTN (car_ ops))
                                                    (let resume (ref (caddr_ d))
                                                        ((((machine (cons_ (car_ s) (car_ d))) (cdddr_ d)) resume) (cadr_ d)))
                                                (if (eq?  'CONS (car_ ops))
                                                    ((((machine (cons_ (cons_ (car_ s) (cadr_ s)) (cddr_ s))) d) (cdr_ ops)) e)

                                                (if (eq? 'SEL (car_ ops))
                                                    (if (eq? (ref (car_ s)) (maybe-lift 0))
                                                        ((((machine (cdr_ s)) (cons_ (cdddr_ ops) d)) (caddr_ ops)) e)
                                                        ((((machine (cdr_ s)) (cons_ (cdddr_ ops) d)) (cadr_ ops)) e))
                                                (if (eq? 'JOIN (car_ ops))
                                                        ((((machine s) (cdr_ d)) (ref (car_ d))) e)
                                                (if (eq? 'MPY (car_ ops))
                                                    ((((machine (cons_ (* (car_ s) (cadr_ s)) (cddr_ s))) d) (cdr_ ops)) e)
                                                (if (eq? 'EQ (car_ ops))
                                                    ((((machine (cons_ (eq? (ref (car_ s)) (ref (cadr_ s))) (cddr_ s))) d) (cdr_ ops)) e)
                                                (if (eq? 'DUM (car_ ops))
                                                    ((((machine s) d) (cdr_ ops)) (cons_ '() e))
                                                (if (eq? 'RAP (car_ ops))
                                                    (let _ (set-car!_ e (cadr_ s))
                                                        (let oldS s
                                                            ((((machine '()) (cons_ (cddr_ oldS) (cons_ e (cons_ (cdr_ ops) d)))) (ref (caar_ oldS))) e)))

                                                (if (eq? 'CAR (car_ ops)) ((((machine (cons_ (car_ (car_ s)) (cdr_ s))) d) (cdr_ ops)) e)
                                                (if (eq? 'CDR (car_ ops)) ((((machine (cons_ (cdr_ (car_ s)) (cdr_ s))) d) (cdr_ ops)) e)
                                                (if (eq? 'QUOTE (car_ ops)) ((((machine (cons_ `(,(car_ s)) (cdr_ s))) d) (cdr_ ops)) e)

                                                (if (eq? 'CADR (car_ ops)) ((((machine (cons_ (cadr_ (car_ s)) (cdr_ s))) d) (cdr_ ops)) e)
                                                (if (eq? 'CADDR (car_ ops)) ((((machine (cons_ (caddr_ (car_ s)) (cdr_ s))) d) (cdr_ ops)) e)
                                                (if (eq? 'CADDDR (car_ ops)) ((((machine (cons_ (cadddr_ (car_ s)) (cdr_ s))) d) (cdr_ ops)) e)

                                                (if (eq? 'EMPTY? (car_ ops))
                                                    ((((machine (cons_ (null? (car_ s)) (cdr_ s))) d) (cdr_ ops)) e)
                                                                    
                                                (if (eq? 'ATOM? (car ops))
                                                    (let a (car_ s)
                                                        (let isAtom (if (sym? a) (maybe-lift 1) (if (num? a) (maybe-lift 1) (maybe-lift 0)))
                                                            ((((machine (cons_ isAtom (cdr_ s))) d) (cdr_ ops)) e)))

                                                (maybe-lift `(ERROR ,(car_ ops)))))))))))))))))))))))))))))
                                        ))))
                        ))

                        (let match (lambda (ops)
                                        (if (eq?  'STOP (car_ ops))
                                            (maybe-lift (lambda (s) (maybe-lift '(Nothing to run))))
                                            (maybe-lift ((((machine '()) '()) ops) '())))
                                    )
                            match)
                    ))
        """

    val evl = s"(let maybe-lift (lambda (e) e) $src)"
    val cmp = s"(let maybe-lift (lambda (e) (lift e)) $src)"

    val evl_curried = s"(let maybe-lift (lambda (e) e) $src_curried)"
    val cmp_curried = s"(let maybe-lift (lambda (e) (lift e)) $src_curried)"

    // Works with cells and without
    val evl_rec_src =   """
            (let dbgCtr 10
                (letrec ((locate (lambda (i j env)
                            (letrec ((loc (lambda (y lst) (if (eq? y 1) (car lst) (loc (- y 1) (cdr lst))))))
                                    (loc j (loc i env))))
                        )
                        (machine (lambda (s d fns ops e)
                                                (if (eq? 'STOP (car ops))
                                                    s
                                                    (let m (lambda (env)
                                                                (if (eq? 'WRITEC (car ops))
                                                                    (car s)
                                                                (if (eq? 'LDC (car ops))
                                                                    (machine (cons (maybe-lift (cadr ops)) s) d fns (cddr ops) env)
                                                                (if (eq? 'ADD (car ops))
                                                                    (machine (cons (+ (car s) (cadr s)) (cddr s)) d fns (cdr ops) env)
                                                                (if (eq? 'SUB (car ops))
                                                                    (machine (cons (- (car s) (cadr s)) (cddr s)) d fns (cdr ops) env)
                                                                (if (eq?  'LD (car ops))
                                                                    (machine (cons (locate (car (cadr ops)) (cadr (cadr ops)) env) s) d fns (cddr ops) env)
                                                                (if (eq?  'LDR (car ops))
                                                                    (machine (cons (locate (car (cadr ops)) (cadr (cadr ops)) fns) s) d fns (cddr ops) env)
                                                                (if (eq? 'LDF (car ops)) 
                                                                    (machine (cons (cons (cadr ops) env) s) d fns (cddr ops) env)
                                                                (if (eq?  'NIL (car ops))
                                                                    (machine (cons (maybe-lift '()) s) d fns (cdr ops) env)
                                                                (if (eq? 'AP (car ops))
                                                                    (machine '() (cons (cddr s) (cons env (cons (cdr ops) d))) fns (caar s) (cons (cadr s) (cdr (car s))))
                                                                (if (eq? 'RTN (car ops))
                                                                    (let resume (caddr d)
                                                                        (machine (cons (car s) (car d)) (cdddr d) fns resume (cadr d)))
                                                                (if (eq?  'CONS (car ops)) (machine (cons (cons (car s) (cadr s)) (cddr s)) d fns (cdr ops) env)

                                                                (if (eq? 'SEL (car ops))
                                                                    (if (eq? (car s) (maybe-lift 0))
                                                                        (machine (cdr s) (cons (cdddr ops) d) fns (caddr ops) env)
                                                                        (machine (cdr s) (cons (cdddr ops) d) fns (cadr ops) env))
                                                                (if (eq? 'JOIN (car ops))
                                                                        (machine s (cdr d) fns (car d) env)
                                                                (if (eq? 'MPY (car ops))
                                                                    (machine (cons (* (car s) (cadr s)) (cddr s)) d fns (cdr ops) env)
                                                                (if (eq? 'EQ (car ops))
                                                                    (machine (cons (eq? (car s) (cadr s)) (cddr s)) d fns (cdr ops) env)
                                                                (if (eq? 'DUM (car ops))
                                                                    (machine s d fns (cdr ops) (cons '() env))
                                                                (if (eq? 'RAP (car ops))
                                                                    (machine '() (cons (cddr s) (cons env (cons (cdr ops) d))) (cons (cadr s) fns) (caar s) env)

                                                                (if (eq? 'CAR (car ops)) (machine (cons (car (car s)) (cdr s)) d fns (cdr ops) env)
                                                                (if (eq? 'CDR (car ops)) (machine (cons (cdr (car s)) (cdr s)) d fns (cdr ops) env)
                                                                (if (eq? 'QUOTE (car ops)) (machine (cons `(,(car s)) (cdr s)) d fns (cdr ops) env)

                                                                (if (eq? 'CADR (car ops)) (machine (cons (cadr (car s)) (cdr s)) d fns (cdr ops) env)
                                                                (if (eq? 'CADDR (car ops)) (machine (cons (caddr (car s)) (cdr s)) d fns (cdr ops) env)
                                                                (if (eq? 'CADDDR (car ops)) (machine (cons (cadddr (car s)) (cdr s)) d fns (cdr ops) env)

                                                                (if (eq? 'EMPTY? (car ops))
                                                                    (machine (cons (null? (car s)) (cdr s)) d fns (cdr ops) env)
                                                                    
                                                                (if (eq? 'ATOM? (car ops))
                                                                    (let a (car_ s)
                                                                        (let isAtom (if (sym? a) (maybe-lift 1) (if (num? a) (maybe-lift 1) (maybe-lift 0)))
                                                                            (machine (cons isAtom (cdr s)) d fns (cdr ops) env)))

                                                                (maybe-lift `(ERROR ,ops))))))))))))))))))))))))))))
                                                        (m e)))
                                        )
                        ))

                        (let match (lambda (ops)
                                        (if (eq?  'STOP (car ops))
                                            (maybe-lift (lambda (s) (maybe-lift '(Nothing to run))))
                                            (maybe-lift (machine '() '() '() ops)))
                                    )
                            match)
                    ))
        """

    val evl_rec = s"(let maybe-lift (lambda (e) e) $evl_rec_src)"

  val evl_secd_src = """
(let debug (lambda (x) (log 0 x))
    (let cdar (lambda (x) (cdr (car x)))
    (let cddr (lambda (x) (cdr (cdr x)))
    (let cdddr (lambda (x) (cdr (cddr x)))
    (let caar (lambda (x) (car (car x)))
    (let null? (lambda (x) (eq? '() x))
    (let map (lambda (f) (letrec ((mapf (lambda (xs) (if (null? xs) xs (cons (f (car xs)) (mapf (cdr xs))))))) mapf))
    (letrec ((mla (lambda (xs) (if (code? xs) xs (if (pair? xs) (maybe-lift (cons (mla (car xs)) (mla (cdr xs)))) (maybe-lift xs))))))
    (let atom? (lambda (a) (if (sym? a) (maybe-lift 1) (if (num? a) (maybe-lift 1) (maybe-lift 0))))
    (letrec ((locate (lambda (i) (lambda (j) (lambda (env)
                (letrec ((loc (lambda (y) (lambda (lst) (if (eq? y 1) (car lst) ((loc (- y 1)) (cdr lst)))))))
                        ((loc j) ((loc i) env))))))))
    (letrec ((machine (lambda (s) (lambda (d) (lambda (fns) (lambda (ops) (lambda (env)
    (if (eq? 'STOP (car ops)) (mla s)
    (if (eq? 'WRITEC (car ops)) (car s)
    (if (eq? 'LDC (car ops))
    (((((machine (cons (maybe-lift (cadr ops)) s)) d) fns) (cddr ops)) env)
    (if (eq? 'ADD (car ops))
    (((((machine (cons (+ (car s) (cadr s)) (cddr s))) d) fns) (cdr ops)) env)
    (if (eq? 'SUB (car ops))
    (((((machine (cons (- (car s) (cadr s)) (cddr s))) d) fns) (cdr ops)) env)
    (if (eq? 'LD (car ops))
    (((((machine (cons (((locate (car (cadr ops))) (cadr (cadr ops))) env) s)) d) fns) (cddr ops)) env)
    (if (eq? 'LDR (car ops))
    (((((machine (cons (lambda (.) (((locate (car (cadr ops))) (cadr (cadr ops))) fns)) s)) d) fns) (cddr ops)) env)
    (if (eq? 'LDF (car ops))
    (((((machine (cons (cons (cadr ops) env) s)) d) fns) (cddr ops)) env)
    (if (eq? 'NIL (car ops))
    (((((machine (cons (maybe-lift '()) s)) d) fns) (cdr ops)) env)
    (if (eq? 'AP (car ops))
    (if (pair? (car s))
    (((((machine '()) (cons (cddr s) (cons env (cons (cdr ops) d)))) fns) (caar s)) (cons (cadr s) (cdr (car s))))
    (let s1 ((car s) 1)
    (let s0 ((car s1) (mla (cons (cadr s) (cdr s1))))
    (let d (cons (cddr s) (cons env (cons (cdr ops) d)))
    (((((machine (cons s0 (car d))) (cdddr d)) fns) (caddr d)) (cadr d))))))
    (if (eq? 'RTN (car ops))
    (if (eq? 'ret d)
    (mla (car s))
    (((((machine (cons (car s) (car d))) (cdddr d)) fns) (caddr d)) (cadr d)))
    (if (eq? 'CONS (car ops))
    (((((machine (cons (cons (car s) (cadr s)) (cddr s))) d) fns) (cdr ops)) env)
    (if (eq? 'SEL (car ops))
    (if (eq? (car s) (maybe-lift 0))
        (((((machine (cdr s)) (cons (cdddr ops) d)) fns) (caddr ops)) env)
        (((((machine (cdr s)) (cons (cdddr ops) d)) fns) (cadr ops)) env))
    (if (eq? 'JOIN (car ops))
    (((((machine s) (cdr d)) fns) (car d)) env)
    (if (eq? 'MPY (car ops))
    (((((machine (cons (* (car s) (cadr s)) (cddr s))) d) fns) (cdr ops)) env)
    (if (eq? 'EQ (car ops))
    (((((machine (cons (eq? (car s) (cadr s)) (cddr s))) d) fns) (cdr ops)) env)
    (if (eq? 'GT (car ops))
    (((((machine (cons (> (car s) (cadr s)) (cddr s))) d) fns) (cdr ops)) env)
    (if (eq? 'DUM (car ops))
    (((((machine s) d) fns) (cdr ops)) (cons '() env))
    (if (eq? 'RAP (car ops))
    (let s1 (cadr s)
    (letrec ((rec (maybe-lift (lambda (env) (((((machine '()) 'ret) (cons (cons (cons rec (cdar s1)) (cdr s1)) fns)) (caar s1)) env)))))
    (((((machine '()) (cons (cddr s) (cons env (cons (cdr ops) d)))) (cons (cons (cons rec (cdar s1)) (cdr s1)) fns)) (caar s)) env)))
    (if (eq? 'CAR (car ops))
    (((((machine (cons (car (car s)) (cdr s))) d) fns) (cdr ops)) env)
    (if (eq? 'CDR (car ops))
    (((((machine (cons (cdr (car s)) (cdr s))) d) fns) (cdr ops)) env)
    (if (eq? 'QUOTE (car ops))
    (((((machine (cons (cons (car s) '()) (cdr s))) d) fns) (cdr ops)) env)
    (if (eq? 'CADR (car ops))
    (((((machine (cons (cadr (car s)) (cdr s))) d) fns) (cdr ops)) env)
    (if (eq? 'CADDR (car ops))
    (((((machine (cons (caddr (car s)) (cdr s))) d) fns) (cdr ops)) env)
    (if (eq? 'CADDDR (car ops))
    (((((machine (cons (cadddr (car s)) (cdr s))) d) fns) (cdr ops)) env)
    (if (eq? 'CDDDR (car ops))
    (((((machine (cons (cdddr (car s)) (cdr s))) d) fns) (cdr ops)) env)
    (if (eq? 'EMPTY? (car ops))
    (((((machine (cons (null? (car s)) (cdr s))) d) fns) (cdr ops)) env)
    (if (eq? 'ATOM? (car ops))
    (((((machine (cons (atom? (car s)) (cdr s))) d) fns) (cdr ops)) env)
    (if (eq? 'SYM? (car ops))
    (((((machine (cons (sym? (car s)) (cdr s))) d) fns) (cdr ops)) env)
    (if (eq? 'NUM? (car ops))
    (((((machine (cons (num? (car s)) (cdr s))) d) fns) (cdr ops)) env)
    (if (eq? 'DBG (car ops))
        (cons 'breakpoint: s)

    (if (eq? 'SAVE (car ops))
    (((((machine s) (cons s d)) fns) (cdr ops)) env)
    (if (eq? 'RSTR (car ops))
    (((((machine (cons (car d) s)) (cdr d)) fns) (cdr ops)) env)
    (maybe-lift (cons 'ERROR ops))
    ))))))))))))))))))))))))))))))))))))))))
    (lambda (ops) (maybe-lift ((((machine '()) '()) '()) ops)))
    )))))))))))
            """
    val secd_src = s"(let maybe-lift (lambda (e) e) $evl_secd_src)"

    def runVM(vmSrc: String, src: String, env: String, runCopmiled: Boolean = true) = {
        if(runCopmiled)
            ev(s"(run 0 (($vmSrc $src) $env))")
        else
            ev(s"(($vmSrc $src) $env)")
    }

    def test() = {
        println("// ------- PE.test --------")

        PETests.basicTests
        PETests.listAccessTest
        PETests.recursionTests
        PETests.factorialTest
        PETests.recTest
        // PETests.curriedVMTest // TODO: @crash

        /*check(ev(s"""(($secd_src '(NIL LDC 1 CONS LDC 10 CONS LDF
                        (DUM NIL LDF
                            (LDC 0 LD (1 1) EQ SEL
                                (LD (1 2) JOIN)
                                (NIL LD (1 2) LD (1 1) MPY CONS
                                        LD (3 2) LD (1 1) SUB CONS LDR (1 1) AP JOIN)
                            RTN)
                        CONS LDF
                            (NIL LD (2 2) CONS LD (2 1) CONS LDR (1 1) AP RTN) RAP
                        RTN) AP WRITEC)) '())"""))("Cst(3628800)")*/

        testDone()
    }
}