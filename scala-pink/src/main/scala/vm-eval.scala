import EVMComp._
import Lisp._
import Base._
import Pink._

object VMEval {

    def meta_eval(p: String, lift: String = "(lambda (x) x)") = s"""
        (letrec (eval) ((lambda (exp env)
                    (if (sym? exp)
                        (env exp)
                    (if (num? exp)
                        ($lift exp)
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
                    (if (eq? (car exp) 'letrec)
                        (let (x)
                            ((eval (caddr exp) env))
                            (eval (cadddr exp) (lambda (z) (if (eq? z (cadr exp)) x (env z)))))
                    (if (eq? (car exp) 'lambda)
                        ($lift (lambda (x) (eval (caddr exp) (lambda (y) (if (eq? y (car (cadr exp))) x (env y))))))
                        
                    ((eval (car exp) env) (eval (cadr exp) env)))))))))))))))
                (eval (quote $p) '()))
    """

  def evalAndRunOnVM(s: String, env: String, pretty: Boolean = false) = {
    val r1 = evalOnVM(meta_eval(s), env)
    val r2 = runOnVM(meta_eval(s), env, pretty = pretty)
    check(r1.toString)(r2.toString)

    // println(Lisp.prettycode(Lam(reifyc(genOnVM(meta_eval(s, "lift"), env)))))

    r1
  }
    def test() = {
        println("// ------- VMEVal.test --------")
        check(evalAndRunOnVM("(- 1 1)", "'()"))("Cst(0)")
        check(evalAndRunOnVM("(if (+ 2 -2) (* 1 -1) (* -1 (* -1 1)))", "'()"))("Cst(1)")
        check(evalAndRunOnVM("(let y 1 (* y 2))", "'()"))("Cst(2)")
        check(evalAndRunOnVM("(let x 2 (let y 3 (+ x y)))", "'()"))("Cst(5)")
        check(evalAndRunOnVM("""(let x0 1 (eq? x0 2))""", "'()"))("Cst(0)")
        check(evalAndRunOnVM("""(let x0 3 (> x0 2))""", "'()"))("Cst(1)")

        check(evalAndRunOnVM("(let y (* 3 2) (+ y 1))", "'()"))("Cst(7)")
        check(evalAndRunOnVM("(let y (lambda (x) (- x 15)) (y 1))", "'()"))("Cst(-14)")
        check(evalAndRunOnVM("((lambda (x) (- x 15)) 1)", "'()"))("Cst(-14)")

        check(evalAndRunOnVM("""((lambda (b) b) 2)""", "'()"))("Cst(2)")
        check(evalAndRunOnVM("""((lambda (b) ((lambda (a) b) 1)) 2)""", "'()"))("Cst(2)")
        check(evalAndRunOnVM("""(((lambda (a) (lambda (b) b)) 1) 2)""", "'()"))("Cst(2)")

        // TODO: record noisy generated code in report before optimizing it out
        check(evalAndRunOnVM("""
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

       ) 6)""", "'()"))("Cst(720)")

        // TODO: add letrec support + CESK (https://cs.indiana.edu/~dyb/pubs/fixing-letrec.pdf)
        check(evalAndRunOnVM("(letrec y (lambda (x) (- x 15)) (y 15))", "'()"))("Cst(0)")
        // check(evalAndRunOnVM("(letrec y (lambda (x) (if (eq? x 0) x (y (- x 1)))) (y 15))", "'()"))("Cst(0)")

        testDone()
    }
}
