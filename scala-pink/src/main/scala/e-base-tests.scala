import EBase._

object TestHelpers {
  import ELisp._

  def evExp(src: Exp) = {
    val ret = inject(src)
    reifyv(ret)
  }

  def checkrunExp(src: Exp, dst: String) = {
    val res = evExp(src)
    check(res)(dst)
    res
  }
  
  def ev(src: String) = {
    val prog_src = src
    val prog_val = parseExp(prog_src)
    val prog_exp = trans(prog_val,Nil)
    evExp(prog_exp)
  }

  def checkrun(src: String, dst: String) = {
    val res = ev(src)
    check(res)(dst)
    res
  }

  var testsRun = 0
  def testDone(): Unit = {
    println(s"  Assertions run: $testsRun"); testsRun = 0
  }

  def check(a:Any)(s:String) = if (a.toString.trim != s.trim) {
    println("error: expected ")
    println("    "+s)
    println("but got")
    println("    "+a)
    (new AssertionError).printStackTrace
  } else testsRun += 1
}

object EBaseTests {
    import TestHelpers._

    def expressionTests() = {
    val exp = Plus(Lit(2), Lit(2))
    checkrunExp(exp, "Cst(4)")

    val exp1 = SetVar(Var("x"), Plus(Var("x"), Var("y")))
    val exp2 = Let(Var("x"), Lit(136), exp1)
    val exp3 = Let(Var("y"), Lit(1), exp2)
    val ret = evalms(State(exp3, initEnv, initStore, Halt())).asInstanceOf[Answer]
    check(ret.s(ret.e("x")))("Cst(137)")

    // x = 136; (\x -> x + y) x
    val funExp = Let(Var("x"), Lit(136), App(
                                Lam(List(Var("y")), // Fun
                                    Plus(Var("x"), Var("y"))),
                                List(Var("x")))) // Args
    checkrunExp(funExp, "Cst(272)")

    val consExp = Let(Var("lst"), Cons(Lit(2), Cons(Lit(3), Lit(4))), Snd(Var("lst")))
    checkrunExp(consExp, "Tup(Cst(3),Cst(4))")

    // set-car!
    val setCarExp = Let(Var("lst"), Cons(Lit(1), Cons(Lit(2), Cons(Lit(3), Lit(4)))),
                        SetVar(Var("lst"), Cons(Lit(0), Snd(Var("lst")))))
    val res = evalms(State(setCarExp, initEnv, initStore, Halt())).asInstanceOf[Answer]
    check(res.s(res.e("lst")))("Tup(Cst(0),Tup(Cst(2),Tup(Cst(3),Cst(4))))")

    // set-cdr!
    val setCdrExp = Let(Var("lst"), Cons(Lit(1), Cons(Lit(2), Cons(Lit(3), Lit(4)))),
                        SetVar(Var("lst"), Cons(Fst(Var("lst")), Lit(0))))
    val res2 = evalms(State(setCdrExp, initEnv, initStore, Halt())).asInstanceOf[Answer]
    check(res2.s(res2.e("lst")))("Tup(Cst(1),Cst(0))")
  }

  def letrecTests() = {
    // Letrec: Simple usage
    val letrecTestExps1 = Letrec(
                          List((Var("x"), Lit(136)), (Var("y"), Lit(1))),
                          Plus(Var("x"), Var("y"))
                        )
    checkrunExp(letrecTestExps1, "Cst(137)")

    val letrecTestExps2 = Letrec(List((Var("f"), Lam(List(Var("ctr")), If(Var("ctr"),
                                                                Plus(Lit(5), Lit(5)),
                                                                Plus(Lit(-5), Lit(-5)))))),
                        App(Var("f"), List(Lit(1))))
    checkrunExp(letrecTestExps2, "Cst(10)")

    // Letrec: Recursive call
    val letrecTestExps3 = Letrec(List((Var("f"), Lam(List(Var("ctr")), If(Var("ctr"),
                                                            App(Var("f"), List(Minus(Var("ctr"), Lit(1)))),
                                                            Plus(Lit(-5), Lit(-5)))))),
                    App(Var("f"), List(Lit(10000))))
    checkrunExp(letrecTestExps3, "Cst(-10)")

    // val letrecTestExps4 = Letrec(
    //                   List((Var("x"), Lit(136)), (Var("y"), Var("x"))),
    //                   Plus(Var("x"), Var("y"))
    //                 )
    // checkrunExp(letrecTestExps4, "Cst(137)")
  }

  def factorialTests() = {
    // factorial
    val facExp = Letrec(List((Var("f"), Lam(List(Var("n")), If(Equ(Var("n"), Lit(1)),
                                                                Lit(1),
                                                                Let(Var("recF"), App(Var("f"), List(Minus(Var("n"), Lit(1)))),
                                                                    Times(Var("n"), Var("recF"))),
                                                                )))),
                    App(Var("f"), List(Lit(12))))
    checkrunExp(facExp, "Cst(479001600)")
  }

  def liftTests() = {
    // Code expressions/Lift operations/Staging
    val liftExp = Lift(Plus(Lit(2), Lit(2)))
    checkrunExp(liftExp, "Code(Lit(4))")

    val liftExp2 = Cons(Lift(Lit(2)), Lift(Lit(2)))
    checkrunExp(liftExp2, "Tup(Code(Lit(2)),Code(Lit(2)))")

    val liftExp3 = Lift(Cons(Lift(Lit(2)), Lift(Lit(2))))
    checkrunExp(liftExp3, "Code(Var(x1))")

    val liftExp4 = Lift(Cons(Lift(Lit(2)), Lift(Plus(Lit(2), Lit(2)))))
    checkrunExp(liftExp4, "Code(Var(x1))")

    val liftExp5 = Lift(Lift(Cons(Lift(Lit(2)), Lift(Plus(Lit(2), Lit(2))))))
    checkrunExp(liftExp5, "Code(Var(x2))") // reflected twice

    val liftExp6 = If(Lift(Lit(0)),Lift(Sym("good")),Lift(Sym("bye")))
    check(reifyc(inject(liftExp6)))("Let(Var(x1),If(Lit(0),Sym(good),Sym(bye)),Var(x1))")

    val liftExp7 = If(Lit(1),Sym("good"),Sym("bye"))
    checkrunExp(liftExp7, "Str(good)")

    val liftExp8 = If(Lift(Plus(Lift(Lit(0)),
                                Lift(Lit(137)))),
                      Lift(Sym("good")),
                      Lift(Sym("bye")))
    checkrunExp(liftExp8, "Code(Var(x3))")

    // Staging set!
    val liftExp9 = 
      Let(Var("x"), Lift(Lit(1)),
        Let(Var("y"), Lift(Lit(136)), SetVar(Var("x"), Plus(Var("y"), Var("x"))))) // TODO: add test where SetVar(x, Run())
    val ret9 = evalms(State(liftExp9, initEnv, initStore, Halt())).asInstanceOf[Answer]
    check(ret9.s(ret9.e("x")))("Code(Var(x6))")
    check(ret9.s(ret9.e("y")))("Code(Lit(136))")
  }

  def runExpTests() = {
    // Run code
      // Generate Run
    val runExp1 = Run(Lift(Lit(0)), Plus(Lift(Lit(136)), Lift(Lit(1))))
    check(reifyc(inject(runExp1)))("Let(Var(x1),Run(Lit(0),Let(Var(x1),Plus(Lit(136),Lit(1)),Var(x1))),Var(x1))") // TODO: make sure variable names in output are sound

      // Run lifted code
    val runExp2 = Run(Lit(0), Plus(Lift(Lit(136)), Lift(Lit(1))))
    checkrunExp(runExp2, "Cst(137)")
  }

  def runCellTests() = {
    val cellExp1 = Cons_(Lit(1), Cons_(Lit(2), Lit(3)))
    println(inject(cellExp1))

    val cellExp2 = Snd_(Cons_(Lit(1), Cons_(Lit(2), Lit(3))))
    println(inject(cellExp2))

    val cellExp3 = Fst_(Cons_(Lit(1), Cons_(Lit(2), Lit(3))))
    println(inject(cellExp3))

    val cellExp4 = Snd_(Snd_(Cons_(Lit(1), Cons_(Lit(2), Lit(3)))))
    println(inject(cellExp4))

    val cellExp5 = Fst_(Fst_(Cons_(Cons_(Lit(-2), Snd_(Cons_(Lit(-3), Lit(-4)))), Cons_(Lit(2), Lit(3)))))
    println(inject(cellExp5))

    val cellExp6 = Fst_(Fst_(Fst_(Fst_(Cons_(Cons_(Lit(-2), Snd_(Cons_(Lit(-3), Lit(-4)))), Cons_(Lit(2), Lit(3))))))) // Note the semantics
    println(inject(cellExp6))

    val cellExp7 = Let(Var("x"),
                       Cons_(Cons_(Lit(-2), Snd_(Cons_(Lit(-3), Lit(-4)))), Cons_(Lit(2), Lit(3))),
                       Plus(Fst_(Fst_(Var("x"))), Snd_(Fst_(Var("x"))))) // -2 + -3
    println(inject(cellExp7))

    val cellExp8 = Let(Var("x"),
                       Cons_(Lit(1), Cons_(Lit(2), Lit(3))),
                       Let(Var("y"),
                           Fst_(Snd_(Var("x"))),
                           Let(Var("_"),
                               SetCar(Var("y"), Lit(4)), // Should change list pointed to by "x"
                               Var("x"))))
    println(inject(cellExp8))

    println(cells)
  }
}