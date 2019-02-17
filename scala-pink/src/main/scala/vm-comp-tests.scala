import EVMComp._
import ELisp._
import EPink._
import EBase._

object EVMCompTests {
    import TestHelpers._

    def basicTests() = {
        check(runOnVM("(+ 5 5)", "'()"))("Cst(10)")
        check(runOnVM("(if (eq? 0 1) (+ 5 5) (- -10 10))", "'()"))("Cst(-20)")
        check(runOnVM("(let (x) (136) (+ x 1))", "'()"))("Cst(137)")
        check(runOnVM("((lambda (x y) (if (eq? x 0) (+ x y) (- x y))) 1 20)", "'()"))("Cst(-19)")
        check(runOnVM("(let (x) ((list 1 2 3 4)) (caddr x))", "'()"))("Cst(3)")
    }

    def evalTest() = {
        check(evalOnVM("""
            (let (eval) ((lambda (ops)
                                (if (eq? (car ops) 5) 5
                                (if (eq? (car ops) 6) 6
                                (if (eq? (car ops) .) .
                                (caddr ops))))))
                            (eval (cons 7 (cons 8 (cons 9 (cons 6 .))))))
        """, "'()"))("Cst(9)")

        check(evalOnVM("""(letrec (eval) ((lambda (ops)
                                                (if (eq? (car ops) 5) 5
                                                (if (eq? (car ops) 6) 6
                                                (if (eq? (car ops) .) .
                                                (eval (cdr ops)))))))
                                            (eval (cons 7 (cons 8 (cons 9 (cons 6 .))))))""", "'()"))("Cst(6)")

        check(runOnVM("""(letrec (eval) ((lambda (ops)
                                                (if (eq? (car ops) 5) 5
                                                (if (eq? (car ops) 6) 6
                                                (if (eq? (car ops) .) .
                                                (eval (cdr ops)))))))
                                            (eval (cons 7 (cons 8 (cons 9 (cons 6 .))))))""", "'()"))("Cst(6)")

        check(runOnVM("""(letrec (eval) ((lambda (ops)
                                                (if (eq? (car ops) 'plus)
                                                    (+ (cadr ops) (caddr ops))
                                                (if (eq? (car ops) 'mul)
                                                    (* (cadr ops) (caddr ops))
                                                (if (eq? (car ops) 'minus)
                                                    (- (cadr ops) (caddr ops))
                                                    .)))))
                                            (eval (list mul 2 3)))""", "'()"))("Cst(6)")

        check(evalOnVM("""(letrec (eval) ((lambda (ops)
                                            (if (atom? ops)
                                                ops
                                                (if (eq? (car ops) 'plus)
                                                    (+ (eval (cadr ops)) (eval (caddr ops)))
                                                    (eval (cdr ops))))))
                                            (eval (list plus 2 (plus 2 2))))""", "'()"))("Cst(6)")

        check(evalOnVM("""(letrec (eval) ((lambda (ops)
                                            (if (atom? ops)
                                                ops
                                                (if (eq? (car ops) 'plus)
                                                    (+ (eval (cadr ops)) (eval (caddr ops)))
                                                    (eval (cdr ops))))))
                                            (eval (list plus 2 (plus 2 2))))""", "'()"))("Cst(6)")
    }

    def factorialTest() = {
        check(
            runOnVM(
                """(letrec (fact)
                        ((lambda (n m) (if (eq? n 0) m (fact (- n 1) (* n m)))))
                            (fact 10 1))""", "'()"))("Cst(3628800)")
    }
}