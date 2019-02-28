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
                    (if (eq? (car exp) 'eq?)
                        (eq? (eval (cadr exp) env) (eval (caddr exp) env))
                    (if (eq? (car exp) '>)
                        (> (eval (cadr exp) env) (eval (caddr exp) env))
                    (if (eq? (car exp) 'if)
                        (if (eval (cadr exp) env) (eval (caddr exp) env) (eval (cadddr exp) env))
                    (if (eq? (car exp) 'let)
                        (let (x)
                            ((eval (caddr exp) env))
                            (eval (cadddr exp) (lambda (z) (if (eq? z (cadr exp)) x (env z)))))
                    (if (eq? (car exp) 'lambda)
                        (lambda (x) (eval (caddr exp) (lambda (y) (if (eq? y (car (cadr exp))) x (env y)))))
                        
                    ((eval (car exp) env) (eval (cadr exp) env))))))))))))))
                (eval (quote $p) '()))
    """

    def test() = {
        println("// ------- VMEVal.test --------")
        check(evalOnVM(meta_eval("(- 1 1)"), "'()"))("Cst(0)")
        check(evalOnVM(meta_eval("(if (+ 2 -2) (* 1 -1) (* -1 (* -1 1)))"), "'()"))("Cst(1)")
        check(evalOnVM(meta_eval("(let y 1 (* y 2))"), "'()"))("Cst(2)") // TODO: runOnVM instead
        check(evalOnVM(meta_eval("(let x 2 (let y 3 (+ x y)))"), "'()"))("Cst(5)")
        check(evalOnVM(meta_eval("""(let x0 1 (eq? x0 2))"""), "'()"))("Cst(0)")
        check(evalOnVM(meta_eval("""(let x0 3 (> x0 2))"""), "'()"))("Cst(1)")

        check(evalOnVM(meta_eval("(let y (* 3 2) (+ y 1))"), "'()"))("Cst(7)") // TODO: @crash
        check(evalOnVM(meta_eval("(let y (lambda (x) (- x 15)) (y 1))"), "'()"))("Cst(-14)") // TODO: @crash
        check(evalOnVM(meta_eval("((lambda (x) (- x 15)) 1)"), "'()"))("Cst(-14)") // TODO: @crash

        check(evalOnVM(meta_eval("""((lambda (b) b) 2)"""), "'()"))("Cst(2)")
        check(evalOnVM(meta_eval("""((lambda (b) ((lambda (a) b) 1)) 2)"""), "'()"))("Cst(2)")
        check(evalOnVM(meta_eval("""(((lambda (a) (lambda (b) b)) 1) 2)"""), "'()"))("Cst(2)")

        check(evalOnVM(meta_eval("""
       (((lambda (fun)
          ((lambda (F)
             (F F))
           (lambda (F)
             (fun (lambda (x) ((F F) x))))))

      (lambda (factorial)
        (lambda (n)
          (if (eq? n 0)
              1
              (* n (factorial (- n 1))))))

       ) 6)"""), "'()"))("Cst(720)")

        testDone()
    }
}
