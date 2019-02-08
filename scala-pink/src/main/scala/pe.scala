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

                                                                (maybe-lift 'ERROR)))))))))))))))))))))))))))
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
                                                    s
                                                (if (eq? 'WRITEC (car_ ops))
                                                    (ref (car_ s))
                                                (if (eq? 'DBG (car_ ops))
                                                    (let toLog (let e s `(stack: ,(caar_ s) car: ,(car_ e) cdr: ,(cdr_ e) car->caddr: ,(caddr_ (car_ e))))
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
                                                        ((((machine '()) (cons_ (cddr_ oldS) (cons_ e (cons_ (cdr_ ops) d)))) (ref (caar_ oldS))) (cons_ (cadr_ oldS) (cdr_ (car_ oldS)))))
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
                                                            (rec (maybe-lift ((((machine '()) (cons_ (cddr_ oldS) (cons_ e (cons_ (cdr_ ops) d)))) (ref (caar_ oldS))) e)))))))

                                                (if (eq? 'CAR (car_ ops)) ((((machine (cons_ (car_ (car_ s)) (cdr_ s))) d) (cdr_ ops)) e)
                                                (if (eq? 'CDR (car_ ops)) ((((machine (cons_ (cdr_ (car_ s)) (cdr_ s))) d) (cdr_ ops)) e)
                                                (if (eq? 'QUOTE (car_ ops)) ((((machine (cons_ `(,(car_ s)) (cdr_ s))) d) (cdr_ ops)) e)

                                                (if (eq? 'CADR (car_ ops)) ((((machine (cons_ (cadr_ (car_ s)) (cdr_ s))) d) (cdr_ ops)) e)
                                                (if (eq? 'CADDR (car_ ops)) ((((machine (cons_ (caddr_ (car_ s)) (cdr_ s))) d) (cdr_ ops)) e)
                                                (if (eq? 'CADDDR (car_ ops)) ((((machine (cons_ (cadddr_ (car_ s)) (cdr_ s))) d) (cdr_ ops)) e)

                                                (if (eq? 'EMPTY? (car_ ops))
                                                    ((((machine (cons_ (null? (car_ s)) (cdr_ s))) d) (cdr_ ops)) e)

                                                (maybe-lift 'ERROR))))))))))))))))))))))))))
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
        // PETests.recursionTests // TODO: @crash
        // PETests.factorialTest // TODO: @crash
        PETests.curriedVMTest // TODO: @crash

        testDone()
    }
}