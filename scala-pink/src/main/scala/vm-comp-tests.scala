import EVMComp._
import Lisp._
import Pink._
import Base._

object EVMCompTests {
    def basicTests() = {
        check(runOnVM("(+ 5 5)", "'()"))("Cst(10)")
        check(runOnVM("(if (eq? 0 1) (+ 5 5) (- -10 10))", "'()"))("Cst(-20)")
        check(runOnVM("(let (x) (136) (+ x 1))", "'()"))("Cst(137)")
        check(runOnVM("((lambda (x y) (if (eq? x 0) (+ x y) (- x y))) 1 20)", "'()"))("Cst(-19)")
        check(runOnVM("(let (x) ((quote (1 2 3 4))) (caddr x))", "'()"))("Cst(3)")
        check(runOnVM("((let (x) ((lambda (z) z)) x) 10)", "'()"))("Cst(10)")
        check(runOnVM("(sym? 'symbol)", "'()"))("Cst(1)")
        check(runOnVM("(sym? '(symbol body))", "'()"))("Cst(0)")
        check(runOnVM("(sym? 1)", "'()"))("Cst(0)")
        check(runOnVM("(letrec (m) ((lambda (s) (if (eq? s 0) 1 (m (- s 1))))) (m 2))", "'()"))("Cst(1)")
//        check(evalOnVM("(letrec (fn) ((lambda (x) (fn 1))) (fn 1))", "'()"))("Cst(1)")
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
                                            (eval (quote (mul 2 3))))""", "'()"))("Cst(6)")

        check(evalOnVM("""(letrec (eval) ((lambda (ops)
                                            (if (atom? ops)
                                                ops
                                                (if (eq? (car ops) 'plus)
                                                    (+ (eval (cadr ops)) (eval (caddr ops)))
                                                    (eval (cdr ops))))))
                                            (eval (quote (plus 2 (plus 2 2)))))""", "'()"))("Cst(6)")

        check(evalOnVM("""(letrec (eval) ((lambda (ops)
                                            (if (atom? ops)
                                                ops
                                                (if (eq? (car ops) 'plus)
                                                    (+ (eval (cadr ops)) (eval (caddr ops)))
                                                    (eval (cdr ops))))))
                                            (eval (quote (plus 2 (plus 2 2)))))""", "'()"))("Cst(6)")
    }

    def factorialTest() = {
        check(
            runOnVM(
                """(letrec (fact)
                        ((lambda (n m) (if (eq? n 0) m (fact (- n 1) (* n m)))))
                            (fact 10 1))""", "'()"))("Cst(3628800)")
    }

    def nestedLambdaTest() = {
        check(
            runOnVM("""
                (let (sum)
                        ((lambda (m)
                            (lambda (n)
                                (+ m n))))
                        ((sum 2) 2))""", "'()"))("Cst(4)")

        check(
            runOnVM("""
                (letrec (sum)
                        ((lambda (m n)
                                (+ m n)))
                        (sum 2 2))""", "'()"))("Cst(4)")
    }

    def ackermannTest() = {
        check(
            runOnVM("""
                (letrec (ack)
                        ((lambda (m n)
                            (if (eq? m 0)
                                (+ n 1)
                                (if (eq? n 0)
                                    (ack (- m 1) 1)
                                    (ack (- m 1) (ack m (- n 1)))))))
                            (ack 3 4))""", "'()"))("Cst(125)")
    }

    def tryFailTest() = {
        check(
            evalOnVM("""(let (x) ((try 1 2 3))
                            (if (< (x) 2)
                                (fail)
                                1))""", "'()"))("Cst(1)") // Note: (x) to force evaluation of try()

        check(
            evalOnVM("""(let (x) ((try 0 1 2 -20))
                            (let (y) ((try 7 8 9))
                                (- (x) (y))))""", "'()"))("Cst(-7)")
    }

    def passFromEnvTest() = {
        check(EVMComp.evalOnVM("""((lambda (x) (+ x p)) 5)""", "'((10))"))("Cst(15)")
        check(EVMComp.evalOnVM("""((lambda (x) (+ x (- p y))) 5)""", "'((10) (11))"))("Cst(4)")
        check(EVMComp.evalOnVM("""
            (let (eval) ((lambda (ops)
                                (if (eq? (car ops) n1) 5
                                (if (eq? (car ops) n2) 6
                                (if (eq? (car ops) null) .
                                (caddr ops))))))
                            (eval (cons 7 (cons 8 (cons 9 (cons 6 .))))))
        """, "'((5) (6) (.))"))("Cst(9)")
        check(EVMComp.runOnVM("""((lambda (x) (+ x p)) 5)""", "'((10))"))("Cst(15)")
        check(EVMComp.runOnVM("""((lambda (x) (+ x (- p y))) 5)""", "'((10) (11))"))("Cst(4)")
        check(EVMComp.runOnVM("""
            (let (eval) ((lambda (ops)
                                (if (eq? (car ops) n1) 5
                                (if (eq? (car ops) n2) 6
                                (if (eq? (car ops) null) .
                                (caddr ops))))))
                            (eval (cons 7 (cons 8 (cons 9 (cons 6 .))))))
        """, "'((5) (6) (.))"))("Cst(9)")
    }
}
