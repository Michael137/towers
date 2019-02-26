import ELisp._
import TestHelpers._

object ELispTests {
    def basicTests() = {
        checkrun("(cadr (cons 1 (cons 2 (+ 5 5))))", "Cst(2)")
        checkrun("(let x (let y 2 (+ y 1)) (let x 2 (+ x x)))", "Cst(4)")
        checkrun("(let x (let y 2 (+ y 1)) (let _ (set! x 136) (+ x 1)))", "Cst(137)")
        // checkrun("(let x (lambda (x y z) (+ x 2)) (x 2))", "Cst(4)") // unused arguments not possible
        checkrun("(let x (lambda (x y z) (+ x 2)) (((x 2) 3) 4))", "Cst(4)")
        checkrun("(let x (lambda (x y z) (+ x 2)) (x 2 3 4))", "Cst(4)")
        checkrun("(letrec ((x 2) (y 3) (z -2)) (+ z x))", "Cst(0)")
        checkrun("(letrec ((f (lambda (x) (if (eq? x 0) (+ x 1) (f (- x 1)))))) (f 15))", "Cst(1)")
        // checkrun("(letrec ((x 2) (y 3) (z (+ x 2))) (+ z x))", "Cst(0)") // TODO
        checkrun("(let lst (cons 1 (cons 2 3)) (let _ (set-car! lst 2) (car lst)))", "Cst(2)")
        checkrun("(let lst (cons 1 (cons 2 3)) (let _ (set-cdr! lst 2) lst))", "Tup(Cst(1),Cst(2))")
        checkrun("(car '(1 2))", "Cst(1)")
        checkrun("(car '(1 2))", "Cst(1)")

        checkrun("((lambda (x) x) 4)", "Cst(4)")
        checkrun("(let y 2 ((lambda (x) y) 4))", "Cst(2)")

        // checkrun("((rlambda f x x) 4)", "Cst(4)")
        // checkrun("((rlambda fn x (if (eq? x 0) 4 (fn 0))) 4)", "Cst(4)")
    }

    def cellTests() = {
        // Mutating cons cells
        checkrun("(let lst (cons_ 1 (cons_ 2 3)) (let _ (set-car!_ lst 2) (* (car_ lst) 1)))", "Cst(2)")
    }

    def quotationTests() = {
        // Quotation
        checkrun("(car_ '(QUOTED))", "Str(QUOTED)")
        checkrun("(cdr_ '(QUOTED))", "Str(.)")

        checkrun("(ref (let lst (cons_ (cons_ 1 2) (cons_ 3 4)) (cadr_ lst)))", "Cst(3)")
        checkrun("""(listref '(1 2 3 4 5))""", "Tup(Cst(1),Tup(Cst(2),Tup(Cst(3),Tup(Cst(4),Tup(Cst(5),Str(.))))))")
        checkrun("""(listref (let lst '(1 2 3 4 5)
                                (cons_ (cons_ (car_ lst) (cadr_ lst)) (cddr_ lst))))""", "Tup(Tup(Cst(1),Cst(2)),Tup(Cst(3),Tup(Cst(4),Tup(Cst(5),Str(.)))))")

        // Quasi-quotation
        checkrun(s"(listref `(,(+ 2 2) 3))", "Tup(Cst(4),Tup(Cst(3),Str(.)))")
        checkrun(s"(listref `(,(+ 2 2) ,3))", "Tup(Cst(4),Tup(Cst(3),Str(.)))")
        checkrun(s"(listref (cons_ `(,(+ 2 2) 3) 4))", "Tup(Tup(Cst(4),Tup(Cst(3),Str(.))),Cst(4))")
    }

    def liftTests() = {
        checkrun("(lift (cadr (cons 1 (cons 2 (+ 5 5)))))", "Code(Lit(2))")
        checkrun("(lift (let x (let y 2 (+ y 1)) (let x 2 (+ x x))))", "Code(Lit(4))")
        checkrun("(lift (let x (let y 2 (+ y 1)) (let _ (set! x 136) (+ x 1))))", "Code(Lit(137))")
        checkrun("(lift (let x (lambda (x y z) (+ x 2)) (x 2 3 4)))", "Code(Lit(4))")

        val compileLamArgs = "(let x (lambda (x) (+ x (lift 2))) (x (lift 2)))"
        checkrun(compileLamArgs, "Code(Var(x3))")
        checkrun(s"(run 0 $compileLamArgs)", "Cst(4)")

        checkrun("(lift (letrec ((x 2) (y 3) (z -2)) (+ z x)))", "Code(Lit(0))")

        checkrun("(lift (let lst (cons 1 (cons 2 3)) (let _ (set-car! lst 2) (car lst))))", "Code(Lit(2))")
        val compileLetrec = "(lift (let lst (lift (cons (lift 1) (lift (cons (lift 2) (lift 3))))) (let _ (set-car! lst (lift 2)) (car lst))))"
        checkrun(s"$compileLetrec", "Code(Var(x6))")
        checkrun(s"(run 0 (run 0 $compileLetrec))", "Cst(2)")

        checkrun("(run 0 (let lst (lift (cons (lift 1) (lift (cons (lift 2) (lift 3))))) (car lst)))", "Cst(1)")
        checkrun("(run 0 (let lst (lift (cons (lift 1) (lift (cons (lift 2) (lift 3))))) (let _ (set-car! lst (lift 2)) (car lst))))", "Cst(2)")
        checkrun("(run 0 (lift (let lst (lift (cons (lift 1) (lift (cons (lift 2) (lift 3))))) (let _ (set-car! lst (lift 2)) (car lst)))))", "Code(Lit(2))")

        // Note interesting side effect: Allows IR with Tup of mixed code and non-code values. Not supported in the original language
        checkrun("(let lst (lift (cons (lift 1) (lift (cons (lift 2) (lift 3))))) (let _ (set-cdr! lst 2) lst))", "Tup(Code(Var(x4)),Cst(2))")

        checkrun("(lift (car '(1 2)))", "Code(Lit(1))")
    }

    def liftCellsTests() = {
        // Staging mutating cells
        checkrun("(lift (ref (let lst (cons_ 1 (cons_ 2 3)) (cadr_ lst))))", "Code(Lit(2))")
        // Note interesting side effect: effectively bypassed the lift() restriction on mixed code/non-code values since we turned tuples into linked lists
        checkrun("(ref (let lst (cons_ (lift 1) (cons_ (lift 2) 3)) (cadr_ lst)))", "Code(Lit(2))")

        checkrun("(listref (let lst (cons_ (lift 1) (lift 2)) lst))", "Tup(Code(Lit(1)),Code(Lit(2)))")
        checkrun("(ref (let lst (cons_ (lift 1) (lift 2)) (car_ lst)))", "Code(Lit(1))")
        checkrun("(ref (let lst (cons_ (lift 1) (lift 2)) (cdr_ lst)))", "Code(Lit(2))")

        checkrun("(run 0 (let lst (lift (cons_ (lift 1) (lift 2))) (car_ lst)))", "Cst(1)")

        check(Base.evalms(Nil, Lisp.trans(Lisp.parseExp("(let lst (cons (lift 1) (lift (cons (lift 2) (lift 3)))) (cadr lst))"), Nil)))("Code(Var(1))")
        check(Base.evalms(Nil, Lisp.trans(Lisp.parseExp("(run 0 (let lst (cons (lift 1) (lift (cons (lift 2) (lift 3)))) (cadr lst)))"), Nil)))("Cst(2)")
        checkrun("(ref (let lst (cons_ (lift 1) (cons_ (lift 2) (lift 3))) (cadr_ lst)))", "Code(Lit(2))")
        checkrun("(run 0 (ref (let lst (cons_ (lift 1) (cons_ (lift 2) (lift 3))) (cadr_ lst))))", "Cst(2)")

        checkrun("(run 0 (lift (let lst (lift (cons_ (lift 1) (cons_ (lift 2) (lift 3)))) (car_ lst))))", "Code(Lit(1))")
        check(Base.evalms(Nil, Lisp.trans(Lisp.parseExp("(run 0 (lift (let lst (lift (cons (lift 1) (lift (cons (lift 2) (lift 3))))) (car lst))))"), Nil)))("Code(Lit(1))")
        check(Lisp.ev("(run 0 (lift (let lst (lift (cons (lift 1) (lift (cons (lift 2) (lift 3))))) (car lst))))"))("Code(Lit(1))")

        // Note semantics: when chaining cons_, should *ONLY* wrap the outer cons_ (if necessary)
        //                 and the individual elements, *NOT* any of the inner cons_
        check(Base.evalms(Nil, Lisp.trans(Lisp.parseExp("(run 0 (let lst (lift (cons (lift 1) (lift (cons (lift 2) (lift 3))))) (cadr lst)))"), Nil)))("Cst(2)")
        checkrun("(run 0 (ref (let lst (lift (cons_ (lift 1) (cons_ (lift 2) (lift 3)))) (cadr_ lst))))", "Cst(2)")

        checkrun("(run 0 (let lst (cons_ (lift 1) (cons_ (lift 2) (lift 3))) (let _ (set-car!_ lst (lift 2)) (* (ref (car_ lst)) (lift 1)))))", "Cst(2)")
        
        // Cons of two cons is consistent between original and new front-end
        checkrun("(ref (let lst (cons_ (cons_ 1 2) (cons_ 3 4)) (cadr_ lst)))", "Cst(3)")
        check(Lisp.ev("(let lst (cons (cons 1 2) (cons 3 4)) (cadr lst))"))("Cst(3)")
    }

    def runTests() = {
        // Run() tests
        checkrunExp(trans(parseExp(s"(run 0 (let x (lift 1) (let y (lift 2) (+ y x))))"), Nil),"Cst(3)")
        checkrunExp(trans(parseExp(s"(run 0 (lift (let x (lift 1) (let y (lift 2) ((lambda (z) (+ z z)) (+ y x))))))"), Nil),"Code(Lit(6))")
        checkrunExp(trans(parseExp(s"(run 0 ((lambda (x) (+ x (lift 10))) (lift 10)))"), Nil),"Cst(20)")
        checkrunExp(trans(parseExp(s"(run 0 (let x (lift 1) (+ x (lift 2))))"), Nil),"Cst(3)")
        checkrunExp(trans(parseExp(s"(run 0 (let x (lambda (x) (* x (lift 2))) (x (lift 3))))"), Nil),"Cst(6)")
        checkrunExp(trans(parseExp(s"(run 0 (let x 10 ((lambda (y) (+ y (lift 2))) (lift 3))))"), Nil),"Cst(5)")
        checkrunExp(trans(parseExp(s"(run 0 (lift (let x (lift '()) (let y (lift 2) (cons y x)))))"), Nil),"Tup(Cst(2),Str(.))")
    }

    def scopingTests() = {
        check(ev("""(((let f_1 (lambda (x) (lambda (y) (if (eq? 'done y)
                                                        (if (eq? x 'done)
                                                            'success
                                                            'failure)
                                                        'failure))
                                )
                            (let f_2 (lambda (x) (if (eq? 'done x)
                                                    (f_1 x)
                                                    (f_1 'nope)))
                                        f_2)) 'not) 'done)"""))("Str(failure)")
    }
}