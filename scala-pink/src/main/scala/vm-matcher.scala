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

    def evalAndRunOnVM(pattern: String, str: String, env: String, pretty: Boolean = true) = {
        val r1 = evalOnVM(matcher(pattern, str), env)
        val r2 = runOnVM(matcher(pattern, str), env, pretty = pretty, max_depth = 80)
        check(r1.toString)(r2.toString)

        // println(Lisp.prettycode(Lam(reifyc(genOnVM(matcher(pattern, str, "lift"), env)))))

        Tup(r1,r2)
    }
    def test() = {
        println("// ------- VMMatcher.test --------")

        println(evalAndRunOnVM("'(a * b done)", "'(b done)", "'()"))
        println(evalAndRunOnVM("'(a * done)", "'(c done)", "'()"))

        testDone()
    }
}
