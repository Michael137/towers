import EVMComp._
import Lisp._
import Base._
import Pink._

object VMLiftedMatcher {

    def lifted_matcher(p: String) = s"""
    (letrec (star_loop) ((lambda (m) (lambda (c) (letrec (inner_loop)
                                            ((lambda (s)
                                                (if (eq? (lift 'yes) (m s)) (lift 'yes)
                                                (if (eq? (lift 'done) (car s)) (lift 'no)
                                                (if (eq? '_ c) (inner_loop (cdr s))
                                                (if (eq? (lift c) (car s)) (inner_loop (cdr s)) (lift 'no)))))))
                                            (lift inner_loop)))))
            (letrec (match_here) ((lambda (r) (lambda (s)
                        (if (eq? 'done (car r))
                            (lift 'yes)
                            (let (m) ((lambda (s)
                                (if (eq? '_ (car r))
                                    (if (eq? (lift 'done) (car s))
                                        (lift 'no)
                                        ((match_here (cdr r)) (cdr s)))
                                    (if (eq? (lift 'done) (car s)) (lift 'no)
                                    (if (eq? (lift (car r)) (car s))
                                        ((match_here (cdr r)) (cdr s))
                                        (lift 'no))))))
                                (if (eq? 'done (car (cdr r))) (m s)
                                (if (eq? '* (car (cdr r)))
                                    (((star_loop (match_here (cdr (cdr r)))) (car r)) s)
                                    (m s))))))))
                        (let (match) ((lambda (r)
                            (if (eq? 'done (car r))
                                (lift (lambda (s) (lift 'yes)))
                                (lift (match_here r)))))
                                (match $p))))
    """

    def printgen(r: String) = {
        println(Lisp.prettycode(Lam(reifyc(genOnVM(r, "'()")))))
    }

    def genMatcher(pattern: String) = {
        printgen(lifted_matcher(pattern))
    }

    def test() = {
        println("// ------- VMLiftedMatcher.test --------")


        genMatcher("'(done)")
        genMatcher("'(a done)")
        genMatcher("'(_ a done)")

        // sanity checks
        printgen("(letrec (rec) ((lambda (s) (+ s s))) (lift rec))")
        printgen("(letrec (rec) ((lambda (s) (if (eq? (lift 'done) s) (lift 0) (rec (cdr s))))) (lift rec))")
        printgen("(letrec (rec) ((lambda (r) (lift (lambda (s) (if (eq? 'done (car r)) s ((rec (cdr r)) s)))))) (rec '(a done)))")
        printgen("(letrec (rec) ((lambda (r) (lambda (s) (if (eq? 'done (car r)) s ((rec (cdr r)) s))))) (lift (rec '(a done))))")
        printgen("(let (top) ((lambda (c) (let (inner) ((lambda (s) (lift c))) (lift inner)))) (top '_))")
        printgen("(let (top) ((lambda (c) (letrec (inner) ((lambda (s) (lift c))) (lift inner)))) (top '_))")

        //genMatcher("'(a * done)")

        testDone()
    }
}
