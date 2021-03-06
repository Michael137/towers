// SECD Instruction Interpreter to be layered above e-stack-vm.scala

object EVMComp {
    // Takes input s-expressions
    // Outputs SECD instructions
    import Lisp._ // For parseExp
    import Lisp.parser._
    import Base._
    import VM._

    type CompEnv = List[List[Val]]

    var indent = 0
    def instrsToString(tup: Val, acc: String = ""): String = (tup match {
        case Tup(Tup(Cst(n1), Tup(Cst(n2), N)), rst) => instrsToString(rst, acc + s" ($n1 $n2) ")
        case Tup(Tup(Str(s1), t2), rst) =>
            var inner = instrsToString(t2, acc + s" ($s1 ")
            instrsToString(rst, inner + ") ")
        case Tup(Cst(n1), rst) => instrsToString(rst, acc + s" $n1 ")
        case Tup(Str(s1), rst) =>
            var str = s" $s1 "
            if(s1 == "LDF" || s1 == "SEL") {
                indent += 1
                str += "\n" + "\t" * indent
            }
            instrsToString(rst, acc + str)
        case N => indent = 0; acc
        case Str(s) => indent = 0; acc + s
    }).replace("  ", " ") // Make it more readable

    def tupToList(t: Val): List[Val] = t match {
        case Tup(t, N) => t::Nil
        case Tup(t1, t2) => t1::tupToList(t2)
    }

    def idx2(e: Val, n: List[Val], j: Int): Int = {
        if(n.size == 0) {
            -1
        } else {
            val hd::tl = n
            if(hd == e) {
                j
            } else {
                idx2(e, tl, j + 1)
            }
        }
    }

    // Keep track of where in environment free variables are stored
    var envArgOffset = 0
    def idx(exp: Val, env: CompEnv, i: Int): Val = {
        if(env.size == 0 || env == Nil) {
            val ret = Tup(Cst(i + envArgOffset), Tup(Cst(1), N))
            envArgOffset = envArgOffset + 1
            ret
        } else {
            val hd::tl = env
            val j = idx2(exp, hd, 1)
            if(j == -1) {
                idx(exp, tl, i + 1)
            } else {
                Tup(Cst(i), Tup(Cst(j), N))
            }
        }
    }

    def index(exp: Val, env: CompEnv) = idx(exp, env, 1)

    def compileLambda(body: Val, env: CompEnv, acc: Val) = {
        Tup(Str("LDF"), Tup(compile(body, env, Tup(Str("RTN"), N)), acc))
    }

    def compileBuiltin(args: Val, env: CompEnv, acc: Val): Val = 
        args match {
            case N => acc
            case Tup(a, b) => compileBuiltin(b, env, compile(a, env, acc))
        }

    def compileIf(cond: Val, conseq: Val, alt: Val, env: CompEnv, acc: Val) = {
        compile(cond, env, Tup(Str("SEL"),
                           Tup(compile(conseq, env, Tup(Str("JOIN"), N)),
                           Tup(compile(alt, env, Tup(Str("JOIN"), N)), acc))))
    }

    def compileApp(args: Val, env: CompEnv, acc: Val): Val = args match {
        case N => acc
        case Tup(hd, tl) => compileApp(tl, env, compile(hd, env, Tup(Str("CONS"), acc)))
    }

    def compileList(args: Val): List[Val] = args match {
      case Tup(hd, tl) => compileList(tl) ++ compileList(hd) ++ List(Str("CONS"))
      case s => List(Str("LDC"), s)
    }
    def toTup(xs: List[Val], acc: Val): Val = xs match {
      case Nil => acc
      case x::xs => Tup(x, toTup(xs, acc))
    }
    def compileList(args: Val, env: CompEnv, acc: Val): Val = {
      toTup(compileList(args), acc)
    }

    def compileTry(args: Val, env: CompEnv, acc: Val): Val = args match {
        // Syntax: (LDF (TRY (exp1* RTN) TRY (exp2* RTN) ... TRY (expN* RTN) FAIL))
        case N => acc
        // case Tup(hd, tl) => Tup(Str("TRY"), compile(hd, env, Tup(Str("RTN"), compileTry(tl, env, acc))))
        case Tup(hd, tl) => Tup(Str("TRY"), Tup(compile(hd, env, Str("RTN")), compileTry(tl, env, acc)))
    }

    var fnEnv: CompEnv = Nil
    var inRec = false
    var hasLDR = false
    def compile(e: Val, env: CompEnv, acc: Val): Val = e match {
        // Null, Number or Identifier
        case N => Tup(Str("NIL"), acc)
        case v: Str => {
            val isRecFn = hasLDR && inRec && fnEnv.flatten.exists( x => x == v)
            val ij = if(isRecFn) index(v, fnEnv) else index(v, env)
            val appStr = if(isRecFn) "LDR" else "LD"
            Tup(Str(appStr), Tup(ij, acc))
        }
        case n: Cst => Tup(Str("LDC"), Tup(n, acc))

        // Builtin, Lambda, Special form

      case Tup(Str("lambda"), Tup(args, Tup(body,N))) =>
            compileLambda(body, tupToList(args)::env, acc)

        case Tup(Str("quote"), Tup(args, _)) =>
            compileList(args, env, Tup(Str(""), acc))

        case Tup(Str("lift"), args) =>
            compileBuiltin(args, env, Tup(Str("LIFT"), acc))

        case Tup(Str("+"), args) =>
            compileBuiltin(args, env, Tup(Str("ADD"), acc))

        case Tup(Str("-"), args) =>
            compileBuiltin(args, env, Tup(Str("SUB"), acc))

        case Tup(Str("*"), args) =>
            compileBuiltin(args, env, Tup(Str("MPY"), acc))

        case Tup(Str(">"), args) =>
            compileBuiltin(args, env, Tup(Str("GT"), acc))

        case Tup(Str("<"), args) =>
            compileBuiltin(args, env, Tup(Str("LT"), acc))

        case Tup(Str("eq?"), args) =>
            compileBuiltin(args, env, Tup(Str("EQ"), acc))

        case Tup(Str("null?"), args) =>
            compileBuiltin(args, env, Tup(Str("EMPTY? CONS"), acc))

        case Tup(Str("atom?"), args) =>
            compileBuiltin(args, env, Tup(Str("ATOM?"), acc))

        case Tup(Str("sym?"), args) =>
            compileBuiltin(args, env, Tup(Str("SYM?"), acc))

        case Tup(Str("num?"), args) =>
            compileBuiltin(args, env, Tup(Str("NUM?"), acc))

        case Tup(Str("car"), args) =>
            compileBuiltin(args, env, Tup(Str("CAR"), acc))

        case Tup(Str("cdr"), args) =>
            compileBuiltin(args, env, Tup(Str("CDR"), acc))

        case Tup(Str("cadr"), args) =>
            compileBuiltin(args, env, Tup(Str("CADR"), acc))

        case Tup(Str("caddr"), args) =>
            compileBuiltin(args, env, Tup(Str("CADDR"), acc))

        case Tup(Str("cadddr"), args) =>
            compileBuiltin(args, env, Tup(Str("CADDDR"), acc))

        case Tup(Str("cdddr"), args) =>
            compileBuiltin(args, env, Tup(Str("CDDDR"), acc))

        case Tup(Str("cons"), args) =>
            compileBuiltin(args, env, Tup(Str("CONS"), acc))

        case Tup(Str("debug"), args) =>
            compileBuiltin(args, env, Tup(Str("DBG"), acc))

        case Tup(Str("fail"), args) =>
            compileBuiltin(args, env, Tup(Str("FAIL"), acc))

        case Tup(Str("if"), Tup(c,Tup(a,Tup(b,N)))) =>
            compileIf(c, a, b, env, acc)

        case Tup(Str("let"), Tup(vs, Tup(vals, Tup(body, N)))) => {
            val newEnv = tupToList(vs)::env
            Tup(Str("NIL"), compileApp(vals, env, compileLambda(body, newEnv, Tup(Str("AP"), acc))))
        }

        case Tup(Str("try"), args) =>
            // Tup(Str("LDF"), Tup(compile(body, env, Tup(Str("RTN"), N)), acc))
            Tup(Str("LDF"), Tup(compileTry(args, env, Tup(Str("FAIL"), N)), acc))

      case Tup(Str("letrec"), Tup(vs, Tup(vals, Tup(body, N)))) => {
           val fnEnvBackup = fnEnv
           val inRecBackup = inRec
           try {
               val newEnv = Nil::env
               inRec = true
               fnEnv = tupToList(vs)::fnEnv
               Tup(Str("DUM"), Tup(Str("NIL"),
                 compileApp(vals, newEnv, compileLambda(body, newEnv, Tup(Str("RAP"), acc)))))
           } finally {
               fnEnv = fnEnvBackup
               inRec = inRecBackup
           }
        }

        // Application
        case Tup(fn, args) => Tup(Str("NIL"), compileApp(args, env, compile(fn, env, Tup(Str("AP"), acc))))
    }

    def teardown() = {
        inRec = false
        fnEnv = Nil
        envArgOffset = 0
    }

    // env and src are passed to PE.runVM in quotes
    // Compilation
    def runOnVM(src: String, env: String, run: Boolean = true, verbose: Boolean = true, pretty: Boolean = true, max_depth: Int = 30, liftEnv: Boolean = false) = {
        hasLDR = true
        val instrs = compile(parseExp(src), Nil, Tup(Str("STOP"), N))
        val instrSrc = instrsToString(instrs)
        if(verbose)
            println("TESTING:\n" + instrSrc)

        if(pretty)
            println(Base.pretty(Base.reifyc(Lisp.ev(s"((${SECD.cmp} '($instrSrc)) (lift '()))")), Nil, max_depth = max_depth))

        val ret = Base.deref(SECD.runVM(SECD.cmp, s"'($instrSrc)", env, run, liftEnv = liftEnv))
        teardown()
        ret
    }

    // Evaluation
    def evalOnVM(src: String, env: String, vm: Any = Nil) = {
        hasLDR = true

        val instrs = compile(parseExp(src), Nil, Tup(Str("STOP"), N))
        val instrSrc = instrsToString(instrs)
        println("TESTING:\n" + instrSrc)

        // val ret = EBase.deref(PE.runVM(PE.evl, s"'($instrSrc)", env, false))
        val ret = Base.deref(SECD.runVM(SECD.evl, s"'($instrSrc)", env, false))
        teardown()
        ret
    }

    def genOnVM(src: String, env: String, vm: Any = Nil) = {
        hasLDR = true

        val instrs = compile(parseExp(src), Nil, Tup(Str("WRITEC"), N))
        val instrSrc = instrsToString(instrs)

        println("TESTING:\n" + instrSrc)

        val ret = Base.deref(SECD.runVM(SECD.evg, s"'($instrSrc)", env, false))
        teardown()
        ret
    }

    def test() = {
        println("// ------- VMComp.test --------")

        EVMCompTests.basicTests
        EVMCompTests.evalTest
        EVMCompTests.factorialTest
        EVMCompTests.nestedLambdaTest
        EVMCompTests.ackermannTest
        EVMCompTests.passFromEnvTest

        testDone()
    }
}
