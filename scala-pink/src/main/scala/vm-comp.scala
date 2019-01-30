// SECD Instruction Interpreter to be layered above e-stack-vm.scala

object EVMComp {
    // Takes input s-expressions
    // Outputs SECD instructions
    import ELisp._ // For parseExp
    import ELisp.parser._
    import EBase._
    import EVM._

    type CompEnv = List[List[Val]]

    def instrsToString(tup: Val, acc: String = ""): String = tup match {
        case Tup(Tup(Cst(n1), Tup(Cst(n2), N)), rst) => instrsToString(rst, acc + s" ($n1 $n2) ")
        case Tup(Tup(Str(s1), t2), rst) =>
            val inner = instrsToString(t2, acc + s" ($s1 ")
            instrsToString(rst, inner + ") ")
        case Tup(Cst(n1), rst) => instrsToString(rst, acc + s" $n1 ")
        case Tup(Str(s1), rst) => instrsToString(rst, acc + s" $s1 ")
        case N => acc
        case Str(s) => acc + s
    }

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

    def compileBuiltin(args: Val, env: CompEnv, acc: Val, quoting: Boolean = false): Val = 
        args match {
            case N => acc
            case Tup(a, b) => compileBuiltin(b, env, compile(a, env, acc, quoting), quoting)
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

    def compile(e: Val, env: CompEnv, acc: Val, quoting: Boolean = false): Val = e match {
        // Null, Number or Identifier
        case N => Tup(Str("NIL"), acc)
        case s: Str => {
          if(quoting) {
            Tup(Str("LDC"), Tup(s, acc))
          } else {
            val ij = index(s, env)
            Tup(Str("LD"), Tup(ij, acc))
          }
        }
        case n: Cst => Tup(Str("LDC"), Tup(n, acc))

        // Builtin, Lambda, Special form

        case Tup(Str("lambda"), Tup(args, Tup(body,N))) =>
            compileLambda(body, tupToList(args)::env, acc)

        case Tup(Str("quote"), args) =>
            compileBuiltin(args, env, Tup(Str("QUOTE"), acc), true)

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

        case Tup(Str("if"), Tup(c,Tup(a,Tup(b,N)))) =>
            compileIf(c, a, b, env, acc)

        case Tup(Str("let"), Tup(vs, Tup(vals, Tup(body, N)))) => {
            val newEnv = tupToList(vs)::env
            Tup(Str("NIL"), compileApp(vals, env, compileLambda(body, newEnv, Tup(Str("AP"), acc))))
        }

        case Tup(Str("letrec"), Tup(vs, Tup(vals, Tup(body, N)))) => {
            val newEnv = tupToList(vs)::env
            Tup(Str("DUM"), Tup(Str("NIL"),
                    compileApp(vals, newEnv, compileLambda(body, newEnv, Tup(Str("RAP"), acc)))))
        }

        // Variable
        case Tup(v: Str, args) =>
            Tup(Str("NIL"), compileApp(args, env, Tup(Str("LD"), Tup(index(v, env), Tup(Str("AP"), acc)))))

        // Application
        case Tup(fn, args) => Tup(Str("NIL"), compileApp(args, env, compile(fn, env, Tup(Str("AP"), acc))))
    }

    // env and src are passed to PE.runVM in quotes
    // Compilation
    def runOnVM(src: String, env: String, run: Boolean = true) = {
        val instrs = compile(parseExp(src), Nil, Tup(Str("STOP"), N))
        // println(instrs)
        val instrSrc = instrsToString(instrs)
        println("TESTING: " + instrSrc)
        deref(PE.runVM(PE.cmp, s"'($instrSrc)", env, run))
    }

    // Evaluation
    def evalOnVM(src: String, env: String) = {
        val instrs = compile(parseExp(src), Nil, Tup(Str("STOP"), N))
        val instrSrc = instrsToString(instrs)
        println("TESTING: " + instrSrc)
        deref(PE.runVM(PE.evl, s"'($instrSrc)", env, false))
    }

    def test() = {
        import TestHelpers._
        println("// ------- VMComp.test --------")

        check(runOnVM("(+ 5 5)", "'()"))("Cst(10)")
        check(runOnVM("(if (eq? 0 1) (+ 5 5) (- -10 10))", "'()"))("Cst(-20)")
        check(runOnVM("(let (x) (136) (+ x 1))", "'()"))("Cst(137)")
        check(runOnVM("((lambda (x y) (if (eq? x 0) (+ x y) (- x y))) 1 20)", "'()"))("Cst(-19)")
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

        // Factorial
        // check(
        //     runOnVM(
        //         """(let (x one) (10 1)
        //             (letrec (fact)
        //                 ((lambda (n m) (if (eq? n 0) one (fact (- n one) (* n m)))))
        //                 (fact x one)))""", "'()"))("Cst(1)") // TODO: revise result

        check(runOnVM("""(letrec (eval) ((lambda (ops)
                                                (if (eq? (car ops) 5) 5
                                                (if (eq? (car ops) 6) 6
                                                (if (eq? (car ops) .) .
                                                (eval (cdr ops)))))))
                                            (eval (cons 7 (cons 8 (cons 9 (cons 6 .))))))""", "'()"))("Cst(6)")

        check(runOnVM("(let (x) ((quote 1 2 3 4)) x)", "'()"))("Cst(1)") // TODO: revise result

        check(runOnVM("""(letrec (eval) ((lambda (ops)
                                                (if (eq? (car ops) 'plus) (+ (cadr ops) (caddr ops))
                                                    .)))
                                            (eval (quote plus 2 2)))""", "'()"))("Cst(6)")

        testDone()
    }
}
