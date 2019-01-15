// Lisp front-end to λ↑↓*
// (not elisp)

object ELisp {
    /* s-expr parser copied from Lisp.scala */
    import EBase._
    import scala.util.parsing.combinator._

    object parser extends JavaTokenParsers with PackratParsers {
        override val whiteSpace = """(\s|(;[^\n]*))+""".r

        def S(x:String) = Str(x)
        def P(x:Val,y:Val) = {
            Tup(x,y)
            // val key = gensym("qcell")
            // cells += (key -> List(x, y))
            // Cell(key, 0)
        }
        def I(x:Int) = Cst(x)
        val N = Str(".")

        lazy val exp: Parser[Val] =
            wholeNumber ^^ { case s => I(s.toInt) } |
            """[^\s\(\)'"]+""".r ^^ { case s => S(s) } |
            "'" ~> exp ^^ { case s => P(S("quote"), P(s, N)) } |
            "()" ^^ { case _ => N } |
            "(" ~> exps <~ ")" ^^ { case vs => vs }

        lazy val exps: Parser[Val] =
            exp ~ opt(exps) ^^ { case v~Some(vs) => P(v, vs) case v~None => P(v,N) }
    }

    import parser._

    def parseExp(s: String) = {
        val Success(v, _) = parseAll(exp, s)
        v
    }

    // Helpers
    import scala.collection.mutable.ListBuffer
    def tupToList(t: Val) = {
        var ret = ListBuffer[String]()
        var tmp = t

        var i = 0
        while(i == 0) {
            tmp match {
                case Tup(a: Str, N) => ret += a.s; i = -1
                case Tup(a: Str, Tup(b, c)) => ret += a.s; tmp = Tup(b, c)
            }
        }
        ret.toList
    }

    def tupToTupList(t: Val) = { // TODO: DRY and cleanup
        var ret = ListBuffer[Val]()
        var tmp = t

        var i = 0
        while(i == 0) {
            tmp match {
                case Tup(a, N) => ret += a; i = -1
                case Tup(a, Tup(b, c)) => ret += a; tmp = Tup(b, c)
            }
        }
        ret.toList
    }

    def trans(e: Val, env: List[String]): Exp = e match {
        case Cst(n) => Lit(n)
        case Str(s) => val i = env.lastIndexOf(s); assert(i>=0, s + " not in " + env); Var(s)
        case Tup(Str("..."),    N)   => Sym("...")
        case Tup(Str("quote"),  Tup(Str(s),N))   => Sym(s)
        case Tup(Str("+"),      Tup(a,Tup(b,N))) => Plus(trans(a,env),trans(b,env))
        case Tup(Str("-"),      Tup(a,Tup(b,N))) => Minus(trans(a,env),trans(b,env))
        case Tup(Str("*"),      Tup(a,Tup(b,N))) => Times(trans(a,env),trans(b,env))
        
        // (let x a b)
        case Tup(Str("let"),    Tup(Str(x),Tup(a,Tup(b,N)))) => Let(Var(x), trans(a,env), trans(b,env:+x))

        case Tup(Str("letrec"), Tup(a, Tup(b, N)))  =>
            val tups = tupToTupList(a)
            val vars = tups.map({ x => 
                val Tup(Str(v), _) = x
                v
            })
            var params = tups.map(
                { x =>
                    val Tup(Str(a), Tup(b, N)) = x
                    (Var(a), trans(b, env:::vars))
                }
            )
            Letrec(params, trans(b, env:::vars))

        case Tup(Str("letrec"), Tup(Tup(Str("..."), N), Tup(body, Tup(varargs, N))))  =>
            VarargLet(trans(varargs, env), trans(body, env))
        
        // (lambda (...) <body with (...) placeholder>)
        case Tup(Str("lambda"), Tup(Tup(Str("..."), N), Tup(body, Tup(varargs,N)))) =>
            VarargLam(trans(varargs, env), trans(body, env))
        
        // (lambda (x1 x2 -> xN) e)
        case Tup(Str("lambda"), Tup(a, Tup(e,N))) =>
            val varnames = tupToList(a)
            val vars = varnames.map({ x: String => Var(x) })
            Lam(vars, trans(e,env:::varnames))

        case Tup(Str("if"),     Tup(c,Tup(a,Tup(b,N)))) => If(trans(c,env),trans(a,env),trans(b,env))
        case Tup(Str("and"),    Tup(a,Tup(b,N))) => And(trans(a,env),trans(b,env))
        case Tup(Str("num?"),   Tup(a,N)) => IsNum(trans(a,env))
        case Tup(Str("sym?"),   Tup(a,N)) => IsStr(trans(a,env))
        case Tup(Str("pair?"),  Tup(a,N)) => IsCons(trans(a,env))
        case Tup(Str("cons"),   Tup(a,Tup(b,N))) => Cons(trans(a,env),trans(b,env))
        case Tup(Str("car"),    Tup(a,N)) => Fst(trans(a,env))
        case Tup(Str("caar"),   Tup(a,N)) => Fst(Fst(trans(a,env)))
        case Tup(Str("cdr"),    Tup(a,N)) => Snd(trans(a,env))
        case Tup(Str("cddr"),   Tup(a,N)) => Snd(Snd(trans(a,env)))
        case Tup(Str("cdddr"),  Tup(a,N)) => Snd(Snd(Snd(trans(a,env))))
        case Tup(Str("cadr"),   Tup(a,N)) => Fst(Snd(trans(a,env)))
        case Tup(Str("caddr"),  Tup(a,N)) => Fst(Snd(Snd(trans(a,env))))
        case Tup(Str("cadddr"), Tup(a,N)) => Fst(Snd(Snd(Snd(trans(a,env)))))
        case Tup(Str("lift"),   Tup(a,N)) => Lift(trans(a,env))
        // case Tup(Str("nolift"), Tup(a,N)) => trans(a,env)
        case Tup(Str("eq?"),    Tup(a,Tup(b,N))) => Equ(trans(a,env),trans(b,env))
        case Tup(Str(">"),      Tup(a,Tup(b,N))) => Gt(trans(a,env),trans(b,env))
        case Tup(Str("set!"),       Tup(a,Tup(b,N))) => SetVar(trans(a,env),trans(b,env))
        case Tup(Str("set-car!"),   Tup(a,Tup(b,N))) => val lst = trans(a,env); SetVar(lst, Cons(trans(b, env), Snd(lst)))
        case Tup(Str("set-cdr!"),   Tup(a,Tup(b,N))) => val lst = trans(a,env); SetVar(lst, Cons(Fst(lst), trans(b, env)))
        case Tup(Str("run"),    Tup(b,Tup(a,N))) => Run(trans(b,env),trans(a,env))
        case Tup(Str("log"),    Tup(b,Tup(a,N))) => Log(trans(b,env),trans(a,env))
        case Tup(Str("quote"),  Tup(a,N)) => Special(benv => a)
        // case Tup(Str("trans"),  Tup(a,N)) =>
        //  Special(benv => Code(trans(evalms(benv, trans(a,env)), env)))
        // case Tup(Str("lift-ref"),Tup(a,N)) =>
        //  Special(benv => Code(Special(b2 => evalms(benv,trans(a,env)))))

        case Tup(Str("cons_"),       Tup(a,Tup(b,N))) => Cons_(trans(a,env),trans(b,env))
        case Tup(Str("car_"),        Tup(a,N)) => Fst_(trans(a,env))
        case Tup(Str("caar_"),       Tup(a,N)) => Fst_(Fst_(trans(a,env)))
        case Tup(Str("cdr_"),        Tup(a,N)) => Snd_(trans(a,env))
        case Tup(Str("cddr_"),       Tup(a,N)) => Snd_(Snd_(trans(a,env)))
        case Tup(Str("cdddr_"),      Tup(a,N)) => Snd_(Snd_(Snd_(trans(a,env))))
        case Tup(Str("cadr_"),       Tup(a,N)) => Fst_(Snd_(trans(a,env)))
        case Tup(Str("caddr_"),      Tup(a,N)) => Fst_(Snd_(Snd_(trans(a,env))))
        case Tup(Str("cadddr_"),     Tup(a,N)) => Fst_(Snd_(Snd_(Snd_(trans(a,env)))))
        case Tup(Str("set-car!_"),   Tup(a,Tup(b,N))) => val lst = trans(a,env); SetCar(lst, trans(b, env))
        case Tup(Str("ref"),         Tup(a, N)) => Ref(trans(a, env))
        case Tup(Str("listref"),     Tup(a, N)) => ListRef(trans(a, env))
                                                    

        case Tup(a, b) =>
            val exps = tupToTupList(b)
            App(trans(a, env), exps.map({ e => trans(e, env) }))
    }

    /*****************
    ***** TESTS ******
    ******************/
    def test() = {
        import TestHelpers._
        println("// ------- ELisp.test --------")
        println("// --> Non-staged <--")

        checkrun("(cadr (cons 1 (cons 2 (+ 5 5))))", "Cst(2)")
        checkrun("(let x (let y 2 (+ y 1)) (let x 2 (+ x x)))", "Cst(4)")
        checkrun("(let x (let y 2 (+ y 1)) (let _ (set! x 136) (+ x 1)))", "Cst(137)")
        checkrun("(let x (lambda (x y z) (+ x 2)) (x 2))", "Cst(4)")
        checkrun("(let x (lambda (x y z) (+ x 2)) (x 2))", "Cst(4)")
        checkrun("(letrec ((x 2) (y 3) (z -2)) (+ z x))", "Cst(0)")
        checkrun("(letrec ((f (lambda (x) (if (eq? x 0) (+ x 1) (f (- x 1)))))) (f 15))", "Cst(1)")
        // checkrun("(letrec ((x 2) (y 3) (z (+ x 2))) (+ z x))", "Cst(0)") // TODO
        checkrun("(let lst (cons 1 (cons 2 3)) (let _ (set-car! lst 2) (car lst)))", "Cst(2)")
        checkrun("(let lst (cons 1 (cons 2 3)) (let _ (set-cdr! lst 2) lst))", "Tup(Cst(1),Cst(2))")
        checkrun("(car '(1 2))", "Cst(1)")
        checkrun("(car '(1 2))", "Cst(1)")

        // Mutating cons cells
        checkrun("(let lst (cons_ 1 (cons_ 2 3)) (let _ (set-car!_ lst 2) (* (car_ lst) 1)))", "Cst(2)")
        checkrun("(car_ '(QUOTED))", "Str(QUOTED)")
        checkrun("(cdr_ '(QUOTED))", "Str(.)")

        checkrun("(ref (let lst (cons_ (cons_ 1 2) (cons_ 3 4)) (cadr_ lst)))", "Cst(3)")
        checkrun("""(listref '(1 2 3 4 5))""", "Tup(Cst(1),Tup(Cst(2),Tup(Cst(3),Tup(Cst(4),Tup(Cst(5),Str(.))))))")
        checkrun("""(listref (let lst '(1 2 3 4 5)
                                (cons_ (cons_ (car_ lst) (cadr_ lst)) (cddr_ lst))))""", "Tup(Tup(Cst(1),Cst(2)),Tup(Cst(3),Tup(Cst(4),Tup(Cst(5),Str(.)))))")

        println("// --> Staged <--")
        checkrun("(lift (cadr (cons 1 (cons 2 (+ 5 5)))))", "Code(Lit(2))")
        checkrun("(lift (let x (let y 2 (+ y 1)) (let x 2 (+ x x))))", "Code(Lit(4))")
        checkrun("(lift (let x (let y 2 (+ y 1)) (let _ (set! x 136) (+ x 1))))", "Code(Lit(137))")
        checkrun("(lift (let x (lambda (x y z) (+ x 2)) (x 2)))", "Code(Lit(4))")

        val compileLamArgs = "(let x (lambda (x y z) (+ x (lift 2))) (x (lift 2)))"
        checkrun(compileLamArgs, "Code(Var(x5))")
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

        checkrun("((lambda (x) x) 4)", "Cst(4)")

        checkrun("(let y 2 ((lambda (x) y) 4))", "Cst(2)")

        testDone()
    }
}