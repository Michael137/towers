// SECD Instruction Interpreter to be layered above e-stack-vm.scala

object EVMComp {
    // Takes input s-expressions
    // Outputs SECD instructions
    import ELisp._ // For parseExp
    import ELisp.parser._
    import EBase._
    import EVM._

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

    def idx(exp: Val, env: CompEnv, i: Int): Val = {
        if(env.size == 0)
            N

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
        val hd::tl = env
        val j = idx2(exp, hd, 1)
        if(j == -1) {
            idx(exp, tl, i + 1)
        } else {
            Tup(Cst(i), Tup(Cst(j), N))
        }
    }

    def index(exp: Val, env: CompEnv) = idx(exp, env, 1)

    def compileLambda(body: Val, env: CompEnv, acc: Val) = 
        Tup(Str("LDF"), Tup(compile(body, env, Tup(Str("RTN"), N)), acc))

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

    def compileList(args: Val, env: CompEnv, acc: Val): Val = args match {
        case N => acc
        case Tup(s: Str, N) => Tup(Str("LDC"), Tup(s, acc))
//      case Tup(fst, (Tup(snd, N))) => Tup(Str("LDC"), Tup(snd, Tup(Str("LDC"), Tup(fst, Tup(Str("CONS"), acc)))))
        case Tup(hd, tl) => compileList(tl, env, Tup(Str("LDC"), Tup(hd, Tup(Str("CONS"), acc))))
    }

    var fnEnv: CompEnv = Nil
    var inRec = false
    var hasLDR = false
    def compile(e: Val, env: CompEnv, acc: Val): Val = e match {
        // Null, Number or Identifier
        case N => Tup(Str("NIL"), acc)
        case s: Str => {        
            val ij = index(s, env)
            Tup(Str("LD"), Tup(ij, acc))
        }
        case n: Cst => Tup(Str("LDC"), Tup(n, acc))

        // Builtin, Lambda, Special form

        case Tup(Str("lambda"), Tup(args, Tup(body,N))) =>
            compileLambda(body, tupToList(args)::env, acc)

        case Tup(Str(s), args) if(s == "list" || s == "quote") =>
            // compileList(args, env, Tup(Str(""), acc))
            Tup(Str("NIL"), compileList(args, env, Tup(Str(""), acc)))

        case Tup(Str("+"), args) =>
            compileBuiltin(args, env, Tup(Str("ADD"), acc))

        case Tup(Str("-"), args) =>
            compileBuiltin(args, env, Tup(Str("SUB"), acc))

        case Tup(Str("*"), args) =>
            compileBuiltin(args, env, Tup(Str("MPY"), acc))

        case Tup(Str(">"), args) =>
            compileBuiltin(args, env, Tup(Str("GT"), acc))

        case Tup(Str("eq?"), args) =>
            compileBuiltin(args, env, Tup(Str("EQ"), acc))

        case Tup(Str("null?"), args) =>
            compileBuiltin(args, env, Tup(Str("EMPTY? CONS"), acc))

        case Tup(Str("atom?"), args) =>
            compileBuiltin(args, env, Tup(Str("ATOM?"), acc))

        case Tup(Str("car"), args) =>
            compileBuiltin(args, env, Tup(Str("CAR"), acc))

        case Tup(Str("cdr"), args) =>
            compileBuiltin(args, env, Tup(Str("CDR"), acc))

        case Tup(Str("cadr"), args) =>
            compileBuiltin(args, env, Tup(Str("CADR"), acc))

        case Tup(Str("caddr"), args) =>
            compileBuiltin(args, env, Tup(Str("CADDR"), acc))

        case Tup(Str("cons"), args) =>
            compileBuiltin(args, env, Tup(Str("CONS"), acc))

        case Tup(Str("debug"), args) =>
            compileBuiltin(args, env, Tup(Str("DBG"), acc))

        case Tup(Str("if"), Tup(c,Tup(a,Tup(b,N)))) =>
            compileIf(c, a, b, env, acc)

        case Tup(Str("let"), Tup(vs, Tup(vals, Tup(body, N)))) => {
            val newEnv = tupToList(vs)::env
            Tup(Str("NIL"), compileApp(vals, env, compileLambda(body, newEnv, Tup(Str("AP"), acc))))
        }

        case Tup(Str("letrec"), Tup(vs, Tup(vals, Tup(body, N)))) => {
            val newEnv = if(hasLDR) { fnEnv = tupToList(vs)::fnEnv; fnEnv } else tupToList(vs)::env
            inRec = true
            Tup(Str("DUM"), Tup(Str("NIL"),
                    compileApp(vals, newEnv, compileLambda(body, newEnv, Tup(Str("RAP"), acc)))))
        }

        // Variable
        case Tup(v: Str, args) =>
            val isRecFn = hasLDR && inRec && fnEnv.flatten.exists( x => x == v)
            val ij = if(isRecFn) index(v, fnEnv) else index(v, env)
            val appStr = if(isRecFn) "LDR" else "LD"
            val app = Tup(Str(appStr), Tup(ij, Tup(Str("AP"), acc)))
            Tup(Str("NIL"), compileApp(args, env, app))

        // Application
        case Tup(fn, args) => Tup(Str("NIL"), compileApp(args, env, compile(fn, env, Tup(Str("AP"), acc))))
    }

    // env and src are passed to PE.runVM in quotes
    // Compilation
    def runOnVM(src: String, env: String, run: Boolean = true) = {
        val instrs = compile(parseExp(src), Nil, Tup(Str("STOP"), N))
        // println(instrs)
        val instrSrc = instrsToString(instrs)
        println("TESTING:\n" + instrSrc)

        hasLDR = true
        // val ret = EBase.deref(PE.runVM(PE.cmp, s"'($instrSrc)", env, run))
        val ret = Base.deref(SECD.runVM(SECD.cmp, s"'($instrSrc)", env, run))
        inRec = false
        ret
    }

    // Evaluation
    def evalOnVM(src: String, env: String, vm: Any = Nil) = {
        val instrs = compile(parseExp(src), Nil, Tup(Str("STOP"), N))
        val instrSrc = instrsToString(instrs)
        println("TESTING:\n" + instrSrc)

        hasLDR = true
        // val ret = EBase.deref(PE.runVM(PE.evl, s"'($instrSrc)", env, false))
        val ret = Base.deref(SECD.runVM(SECD.evl, s"'($instrSrc)", env, false))
        inRec = false
        ret
    }

    def test() = {
        import TestHelpers._
        println("// ------- VMComp.test --------")

        EVMCompTests.basicTests
        EVMCompTests.evalTest
        EVMCompTests.factorialTest

        testDone()
    }
}