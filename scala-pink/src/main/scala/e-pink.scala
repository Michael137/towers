// Meta-circular stage-parametric interpreter for Pink

import EBase._
import ELisp._
import TestHelpers._

object EPinkBase {
    val fac_src = "(lambda (m) (letrec ((f (lambda (n) (if (eq? n 0) 1 (* n (f (- n 1))))))) (f m)))"
    val fac_val = parseExp(fac_src)
    val fac_exp = trans(fac_val, List("arg"))
    def ev_nolift(src: String) = s"""(lambda (expr)
                                        (letrec ((eval (lambda (expr env)
                                                        ($src (lambda (expr) expr) eval expr env))))
                                                (eval expr 'nil-env)))"""
    def ev_lift(src: String) = s"""(lambda (expr)
                                        (letrec ((eval (lambda (expr env)
                                                        ($src (lambda (expr) (lift expr)) eval expr env))))
                                                (eval expr 'nil-env)))"""
}
import EPinkBase._

object EPink {
    //TODO: set!/set-car!/cells
    val ev_poly_src = """
        (lambda (maybe-lift eval exp env)
            (if (num?                exp)    (maybe-lift exp)
            (if (sym?                exp)    (env exp)
            (if (sym?           (car exp))
                (if (eq?  '+      (car exp))   (+   (eval (cadr exp) env) (eval (caddr exp) env))
                (if (eq?  '-      (car exp))   (-   (eval (cadr exp) env) (eval (caddr exp) env))
                (if (eq?  '*      (car exp))   (*   (eval (cadr exp) env) (eval (caddr exp) env))
                (if (eq?  'eq?    (car exp))   (eq? (eval (cadr exp) env) (eval (caddr exp) env))
                (if (eq?  '>    (car exp))   (> (eval (cadr exp) env) (eval (caddr exp) env))
                (if (eq?  'if     (car exp))   (if  (eval (cadr exp) env) (eval (caddr exp) env) (eval (cadddr exp) env))
                (if (eq?  'lift   (car exp))   (lift (eval (cadr exp) env))
                (if (eq?  'num?   (car exp))   (num? (eval (cadr exp) env))
                (if (eq?  'sym?   (car exp))   (sym? (eval (cadr exp) env))
                (if (eq?  'lambda (car exp))   (lambda (...) (eval (caddr exp) (...)) (cadr exp))
                (if (eq?  'let    (car exp))   (let x (eval (caddr exp) env)
                                                    (eval (cadddr exp) (lambda (z)
                                                                            (if (eq?  z (cadr exp))
                                                                                x
                                                                                (env z)))))
                (if (eq?  'letrec (car exp))   (letrec (...) (eval (caddr exp) (...)) (cadr exp))
                (if (eq?  'car    (car exp))   (car  (eval (cadr exp) env))
                (if (eq?  'caar    (car exp))  (caar (eval (cadr exp) env))
                (if (eq?  'cdr    (car exp))   (cdr  (eval (cadr exp) env))
                (if (eq?  'cddr   (car exp))   (cddr (eval (cadr exp) env))
                (if (eq?  'cdddr  (car exp))   (cdddr (eval (cadr exp) env))
                (if (eq?  'cadr   (car exp))   (cadr (eval (cadr exp) env))
                (if (eq?  'caddr  (car exp))   (caddr (eval (cadr exp) env))
                (if (eq?  'cadddr (car exp))   (cadddr (eval (cadr exp) env))
                (if (eq?  'cons   (car exp))   (maybe-lift (cons (eval (cadr exp) env) (eval (caddr exp) env)))
                (if (eq?  'quote  (car exp))   (maybe-lift (cadr exp))
                (if (eq?  'pair?  (car exp))   (pair? (eval (cadr exp) env))
                (if (eq?  'run    (car exp))   (run (eval (cadr exp) env) (eval (caddr exp) env))
                (if (eq?  'log    (car exp))   (log (eval (cadr exp) env) (eval (caddr exp) env))
                ((log 0 (env (car exp))) (eval (cadr exp) env)))))))))))))))))))))))))))
            ((eval (car exp) env) (eval (cadr exp) env))))))
        """

    val eval_src = ev_nolift(ev_poly_src)
    val eval_val = parseExp(eval_src)
    val eval_exp1 = trans(eval_val, List("arg1"))

    val evalc_src = ev_lift(ev_poly_src)
    val evalc_val = parseExp(evalc_src)
    val evalc_exp1 = trans(evalc_val, List("arg1"))

    def test() = {
        println("// ------- EPink.test --------")

        // inject(trans(parseExp(ev_poly_src), Nil))
        // println(fac_exp)
        // println(eval_src)
        // println(eval_val)
        // println(eval_exp1)
        // println(evalc_exp1)
        
        // println(eval_src)
        testCorrectnessOptimality()
    }

    def testCorrectnessOptimality() = {
        // TODO  implement epink + stage the machine + write the compiler (1st is to construct base-pink-vm tower while second is to construct base-vm-eval tower)
        //       construction of more elaborate towers/VMs for different purposes with different features
        //       heterogeneous data representation staging e.g. through (i.e. bypassing) VM instructions to base language
        //       check compiled output and compare compilation of towers. what are the overhead patterns?

        // println(Lisp.ev("(let x (cons 1 2) (car x))"))
        // println(ev("(let x (cons 1 2) (car x))"))

        // direct execution
        checkrun(s"""
        (let fac $fac_src 
            (fac 4))""",
            "Cst(24)")

        // Simple interpretation
        checkrun(s"""
            (let eval $eval_src
                (eval (quote (+ 4 4))))
        """, "Cst(8)")

        checkrun(s"""
        (let eval $eval_src
            (let src (quote (lambda (y) (+ y 1)))
                ((eval src) 4)))""",
        "Cst(5)")

        checkrun(s"""
        (let eval $eval_src
            (let src (quote (lambda (y x) (+ x x)))
                ((eval src) 4 5)))""",
        "Cst(10)")

        checkrun(s"""
        (let eval $eval_src
            (let src (quote (let y 1 (+ y 1)))
                (eval src)))""",
        "Cst(2)")

        checkrun(s"""
        (let eval $eval_src
            (let src (quote (letrec ((y 1) (x 2)) (+ y x)))
                (eval src)))""",
        "Cst(3)")

        checkrun(s"""
        (let eval $eval_src
            (let src (quote (letrec ((y 1) (x 2) (z 3)) (+ x (- z y))))
                (eval src)))""",
        "Cst(4)")

        checkrun(s"""
        (let eval $eval_src
            (let src (quote (letrec ((y 4)) (cons y 2)))
                (eval src)))""",
        "Tup(Cst(4),Cst(2))")

        // // TODO: works with (log 0!
        // checkrun(s"""
        // (let eval $eval_src
        //     (let src (quote (let y (lambda (m) (+ m 2)) (y 1)))
        //         (eval src)))""",
        // "Cst(3)")
        // checkrun(s"""
        // (let eval $eval_src
        //     (let src (quote (let y (lambda (m) (+ m 2)) (y 1)))
        //         ((eval src) 9)))""",
        // "Cst(11)")
        // checkrun(s"""
        // (let eval $eval_src
        //     (let src (quote (let y (lambda (m) (+ m 2)) (+ 10 1)))
        //         (eval src)))""",
        // "Cst(11)")

        // checkrun(s"""
        // (let eval $eval_src
        //     (let src (quote (letrec ((y (lambda (m) (+ m 2)))) (y 1)))
        //         (eval src)))""",
        // "Cst(11)")

        // checkrun(s"""
        // (let eval $eval_src
        //     (let src (quote ((lambda (x) (* x x)) 40))
        //         (eval src)))""",
        // "Cst(2)")

        // interpretation
        // checkrun(s"""
        // (let eval $eval_src
        //     (let fac_src '($fac_src)
        //         (eval fac_src)))""",
        // "Cst(24)")
    }
}