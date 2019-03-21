import EVMComp._
import Lisp._
import Base._
import Pink._

object VMMatcher {

    def matcher1(p: String, str: String, lift: String = "(lambda (x) x)") = s"""
(letrec (match) ((lambda (r s)
(if (eq? 'done (car r))
    'yes
(if (eq? '_ (car r))
  (if (eq? 'done (car s))
      'no
      (match (cdr r) (cdr s))
      )
(if (eq? 'done (car s))
  'no
(if (eq? (car r) (car s))
    (match (cdr r) (cdr s))
    'no))))))
(match $p $str))
    """
    def matcher(p: String, str: String, lift: String = "(lambda (x) x)") = s"""
    (letrec (star_loop) ((lambda (m c) (letrec (inner_loop)
                                            ((lambda (s)
                                                (if (eq? 'yes (m s)) 'yes
                                                (if (eq? 'done (car s)) 'no
                                                (if (eq? '_ c) (inner_loop (cdr s))
                                                (if (eq? c (car s)) (inner_loop (cdr s)) 'no))))))
                                            inner_loop)))
            (letrec (match_here) ((lambda (r s)
                        (if (eq? 'done (car r))
                            'yes
                            (let (m) ((lambda (s)
                                (if (eq? '_ (car r))
                                    (if (eq? 'done (car s))
                                        'no
                                        (match_here (cdr r) (cdr s)))
                                    (if (eq? 'done (car s)) 'no
                                    (if (eq? (car r) (car s))
                                        (match_here (cdr r) (cdr s))
                                        'no)))))
                                (if (eq? 'done (car (cdr r))) (m s)
                                (if (eq? '* (car (cdr r)))
                                    (lambda (x) 'random)
                                    (m s)))))))
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

        check(evalOnVM(
            """(letrec (rec) ((lambda (arg)
                                (letrec (rec2)
                                    ((lambda (arg2)
                                        (+ arg arg2))) rec2))) ((rec 1) 2))""", "'()"))("Cst(3)")

        println(evalOnVM(matcher1("'(done)", "'(done)"), "'()"))
        println(evalOnVM(matcher("'(_ * a _ * done)", "'(b a done)"), "'()"))
        println(evalOnVM(matcher("'(done)", "'(done)"), "'()"))

        testDone()
    }
}


// TODO: progress with matcher
// TODO: note down try/fail logic
