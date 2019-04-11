import EVMComp._
import Lisp._
import Base._
import Pink._

object VMMatcher {

    def matcher(p: String, str: String, lift: String = "(lambda (x) x)") = s"""
    (letrec (star_loop) ((lambda (m) (lambda (c) (letrec (inner_loop)
                                            ((lambda (s)
                                                (if (eq? 'yes (m s)) 'yes
                                                (if (eq? 'done (car s)) 'no
                                                (if (eq? '_ c) (inner_loop (cdr s))
                                                (if (eq? c (car s)) (inner_loop (cdr s)) 'no))))))
                                            inner_loop))))
            (letrec (match_here) ((lambda (r) (lambda (s)
                        (if (eq? 'done (car r))
                            'yes
                            (let (m) ((lambda (s)
                                (if (eq? '_ (car r))
                                    (if (eq? 'done (car s))
                                        'no
                                        ((match_here (cdr r)) (cdr s)))
                                    (if (eq? 'done (car s)) 'no
                                    (if (eq? (car r) (car s))
                                        ((match_here (cdr r)) (cdr s))
                                        'no)))))
                                (if (eq? 'done (car (cdr r))) (m s)
                                (if (eq? '* (car (cdr r)))
                                    (((star_loop (match_here (cdr (cdr r)))) (car r)) s)
                                    (m s))))))))
                        (let (match) ((lambda (r)
                            (if (eq? 'done (car r))
                                (lambda (s) 'yes)
                                (match_here r))))
                                ((match $p) $str))))
    """

    def evalAndRunOnVM(pattern: String, str: String, env: String) = {
        val r1 = evalOnVM(matcher(pattern, str), env)
        val r2 = runOnVM(matcher(pattern, str), env)
        check(r1.toString)(r2.toString)

        // println(Lisp.prettycode(Lam(reifyc(genOnVM(matcher(s, "lift"), env)))))

        r1
    }
    def test() = {
        println("// ------- VMMatcher.test --------")

        //println(evalOnVM(matcher("'(_ * a _ * done)", "'(b a done)"), "'()"))
        //check(runOnVM(
        //    """(letrec (rec) ((lambda (arg)
        //                       (+ arg 2))) (rec 1))""", "'()"))("Cst(3)")
        //check(runOnVM(
        //    """(letrec (rec) ((lambda (arg)
        //                        (let (rec2)
        //                            ((lambda (arg2)
        //                                (+ arg arg2))) rec2))) ((rec 1) 2))""", "'()"))("Cst(3)")
        // check(runOnVM(
        //     """(letrec (rec) ((lambda (arg)
        //                         (letrec (rec2)
        //                             ((lambda (arg2)
        //                                 (+ arg arg2))) rec2))) ((rec 1) 2))""", "'()"))("Cst(3)")

        println(runOnVM(matcher("'(_ * a _ * done)", "'(b a done)"), "'()"))

        println(ev(s"""
        ((${SECD.evl} '(
            DUM NIL LDF
                    (DUM NIL
                        LDF (LD (1 1) LD (3 1) ADD RTN) CONS
                            LDF (LDR (1 1) RTN)
                    RAP RTN)
                    CONS LDF
                        (NIL LDC 2 CONS NIL LDC 1 CONS LDR (1 1) AP AP RTN) RAP WRITEC
        )) '())"""))

        /*
            Generated closure:
                Let(Lam(Let(Lam(Let(Lam(Let(Fst(Var(5)),Let(Fst(Var(6)),Let(Snd(Var(5)),Let(Snd(Var(8)),Let(Fst(Var(9)),Let(Fst(Var(10)),Let(Plus(Var(11),Var(7)),Let(App(Var(12),Lit(1)),Var(13)))))))))),Let(Cons(Sym("."),Var(3)),Let(Cons(Var(4),Var(5)),Var(6))))),Let(Cons(Lit(1),Sym(".")),Let(Cons(Sym("."),Var(1)),Let(Cons(Var(3),Var(4)),Let(App(Var(2),Var(5)),Let(Fst(Var(6)),Let(Snd(Var(6)),Let(Snd(Var(8)),Let(Cons(Lit(2),Sym(".")),Let(Cons(Var(10),Var(9)),Let(App(Var(7),Var(11)),Var(12))))))))))))),Var(0))
        */

        // after LDR 'ret path should not be taken in RTN
        //println(evalms(Nil, App(Let(Lam(Let(Lam(Let(Lam(Let(Fst(Var(5)),Let(Fst(Var(6)),Let(Snd(Var(5)),Let(Snd(Var(8)),Let(Fst(Var(9)),Let(Var(10),Let(Plus(Var(11),Var(7)),Let(App(Var(12),Lit(1)),Var(13)))))))))),Let(Cons(Sym("."),Var(3)),Let(Cons(Var(4),Var(5)),Var(6))))),Let(Cons(Lit(1),Sym(".")),Let(Cons(Sym("."),Var(1)),Let(Cons(Var(3),Var(4)),Let(App(Var(2),Var(5)),Let(Fst(Var(6)),Let(Snd(Var(6)),Let(Snd(Var(8)),Let(Cons(Lit(2),Sym(".")),Let(Cons(Var(10),Var(9)),Let(App(Var(7),Var(11)),Var(12))))))))))))),Var(0)), Sym(".")), force_log = true))

        testDone()
    }
}
