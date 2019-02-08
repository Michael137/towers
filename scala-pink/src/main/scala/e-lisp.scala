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
            """[^\s\(\)'`,"]+""".r ^^ { case s => S(s) } |
            "'" ~> exp ^^ { case s => P(S("quote"), P(s, N)) } |
            "`" ~> exp ^^ { case s => P(S("quasi"), P(s, N)) } |
            "," ~> exp ^^ { case s => P(S("unquote"), P(s, N)) } |
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
        case Tup(Str("quasi"),  Tup(Str(s),N))   => Sym(s)
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
        case Tup(Str("null?"),  Tup(a,N)) => IsNull(trans(a, env))
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
        case Tup(Str("quasi"),  Tup(a,b)) => 
            val exps = tupToTupList(a)
            val transed = exps.map({ a => a match {
                case Tup(Str("unquote"), Tup(rst, N)) => trans(rst, env)
                case other => Special(benv => other)
            }})

            def aux(lst: List[Exp]): Exp = lst match {
                case hd::tl => Cons(hd, aux(tl))
                case Nil => Sym(".")
            }
            aux(transed)

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

        case Tup(Str("rec"),         Tup(a, N)) =>
            AppRec(trans(a, env))

        case Tup(a, b) =>
            val exps = tupToTupList(b)
            App(trans(a, env), exps.map({ e => trans(e, env) }))
    }

    /*****************
    ***** TESTS ******
    ******************/
    def test() = {
        import ELispTests._
        import TestHelpers._
        println("// ------- ELisp.test --------")
        println("// --> Non-staged <--")
        basicTests()
        cellTests()
        quotationTests()

        println("// --> Staged <--")
        liftTests()
        liftCellsTests()
        runTests()
        
        println("// --> Mixed <--")
        scopingTests()

        testDone()
    }
}