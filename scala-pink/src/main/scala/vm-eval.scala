import EVMComp._
import Lisp._
import Base._
import Pink._

object VMEval {

    def meta_eval(p: String) = s"""
        (letrec (eval) ((lambda (exp env)
                    (if (sym? exp)
                        (env exp)
                    (if (num? exp)
                        exp
                    (if (eq? (car exp) '+)
                        (+ (eval (cadr exp) env) (eval (caddr exp) env))
                    (if (eq? (car exp) '-)
                        (- (eval (cadr exp) env) (eval (caddr exp) env))
                    (if (eq? (car exp) '*)
                        (* (eval (cadr exp) env) (eval (caddr exp) env))
                    (if (eq? (car exp) 'if)
                        (if (eval (cadr exp) env) (eval (caddr exp) env) (eval (cadddr exp) env))
                    (if (eq? (car exp) 'let)
                        (let (x)
                            ((eval (caddr exp)))
                            (eval (cadddr exp) (lambda (z) (if (eq? z (cadr exp)) x (env z)))))
                    (if (eq? (car exp) 'lambda)
                        (lambda (x) (eval (caddr exp) (lambda (y) (if (eq? y (cadr exp)) x (env y)))))
                        
                    (eval (cdr exp) env)))))))))))
                (eval (list $p) '()))
    """

    def test() = {
        println("// ------- VMEVal.test --------")
        check(evalOnVM("(+ 5 5)", "'()"))("Cst(10)") // TODO: !! removing this line breaks the test (when run on its own)
        check(evalOnVM(meta_eval("(- 1 1)"), "'()"))("Cst(0)")
        check(evalOnVM(meta_eval("(if (+ 2 -2) (* 1 -1) (* -1 (* -1 1)))"), "'()"))("Cst(1)")
        check(evalOnVM(meta_eval("(let y 1 (* y 2))"), "'()"))("Cst(2)") // TODO: runOnVM instead
        check(evalOnVM(meta_eval("(let y (lambda (x) (- x 15)) (y 1))"), "'()"))("Cst(-14)")
        check(evalOnVM(meta_eval("((lambda (x) (- x 15)) 1)"), "'()"))("Cst(-14)")

        testDone()
    }
}