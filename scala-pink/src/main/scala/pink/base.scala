// multi-level core language λ↑↓ as a definitional interpreter in Scala

object Base {
  var log: Val => Unit = {x => println(x); println}

  // expressions
  abstract trait ListOp
  abstract class Exp
  case class Lit(n:Int) extends Exp
  case class Sym(s:String) extends Exp
  case class Var(n:Int) extends Exp
  case class App(e1:Exp, e2:Exp) extends Exp
  case class Lam(e:Exp) extends Exp
  case class Let(e1:Exp,e2:Exp) extends Exp
  case class If(c:Exp,a:Exp,b:Exp) extends Exp
  case class Plus(a:Exp,b:Exp) extends Exp
  case class Minus(a:Exp,b:Exp) extends Exp
  case class Times(a:Exp,b:Exp) extends Exp
  case class Equ(a:Exp,b:Exp) extends Exp
  case class Gt(a:Exp,b:Exp) extends Exp
  case class Lt(a:Exp,b:Exp) extends Exp
  case class Or(a:Exp,b:Exp) extends Exp
  case class And(a:Exp,b:Exp) extends Exp
  case class Not(a:Exp) extends Exp
  case class Cons(a:Exp,b:Exp) extends Exp
  case class Fst(a:Exp) extends Exp with ListOp
  case class Snd(a:Exp) extends Exp with ListOp
  case class IsNum(a:Exp) extends Exp
  case class IsStr(a:Exp) extends Exp
  case class IsCons(a:Exp) extends Exp
  case class IsCode(a:Exp) extends Exp
  case class IsClosure(a:Exp) extends Exp
  case class Lift(e:Exp) extends Exp
  case class Run(b:Exp,e:Exp) extends Exp
  case class Log(b:Exp,e:Exp) extends Exp

  // for custom extensions to AST
  case class Special(f:Env => Val) extends Exp {
    override def toString = "<special>"
  }

  // values
  type Env = List[Val]

  abstract class Val
  case class Cst(n:Int) extends Val
  case class Str(s:String) extends Val
  case class Clo(env:Env,e:Exp) extends Val {
    override def toString = s"Clo(_, $e)"
  }
  case class Tup(v1:Val,v2:Val) extends Val

  case class Code(e:Exp) extends Val

  // interpreter state and mechanics
  var stFresh = 0
  var stBlock: List[(Int, Exp)] = Nil
  // var stBlock: List[Exp] = Nil
  var stFun: List[(Int,Env,Exp)] = Nil
  def run[A](f: => A): A = {
    val sF = stFresh
    val sB = stBlock
    val sN = stFun
    try f finally { stFresh = sF; stBlock = sB; stFun = sN }
  }

  def fresh() = {
    stFresh += 1; Var(stFresh-1)
  }
  def reify(f: => Exp) = run {
    stBlock = Nil
    val last = f
    (stBlock.map(_._2) foldRight last)(Let)
  }
  def reflect(s:Exp) = {
    // CSE trick would optimize out side-effecting operations
    // Thus create a match guard to avoid this
    stBlock.find(_._2 == s) match {
      case Some ((i, exp)) if(!exp.isInstanceOf[Log]) => Var(i)
      case _ =>
        s match {
          // smart constructors
          case Fst(Cons(a, b)) => a
          case Snd(Cons(a, b)) => b
          case Plus(Lit(a), Lit(b)) => Lit(a + b)
          case Minus(Lit(a), Lit(b)) => Lit(a - b)
          case Times(Lit(a), Lit(b)) => Lit(a * b)
          case _ =>
            stBlock :+= (stFresh, s)
            fresh()
        }
    }
  }

  def deref(a: Val): Val = a match {
    case Tup(a, _) => a
    case other => other
  }

  // anf conversion: for checking generated against expected code
  def anf(env: List[Exp], e: Exp): Exp = e match {
    case Lit(n) => Lit(n)
    case Sym(n) => Sym(n)
    case Var(n) => env(n)
    case App(e1,e2) =>
      reflect(App(anf(env,e1),anf(env,e2)))
    case Lam(e) => 
      reflect(Lam(reify(anf(env:+fresh():+fresh(),e))))
    case Let(e1,e2) => 
      anf(env:+(anf(env,e1)),e2)
    case If(c,a,b) => 
      reflect(If(anf(env,c),reify(anf(env,a)),reify(anf(env,b))))
    case Plus(e1,e2) =>
      reflect(Plus(anf(env,e1),anf(env,e2)))
    case Times(e1,e2) =>
      reflect(Times(anf(env,e1),anf(env,e2)))
    case Minus(e1,e2) =>
      reflect(Minus(anf(env,e1),anf(env,e2)))
    case Equ(e1,e2) =>
      reflect(Equ(anf(env,e1),anf(env,e2)))
    case Gt(e1,e2) =>
      reflect(Gt(anf(env,e1),anf(env,e2)))
    case Lt(e1,e2) =>
      reflect(Lt(anf(env,e1),anf(env,e2)))
    case Or(e1,e2) =>
      reflect(Or(anf(env,e1),anf(env,e2)))
    case And(e1,e2) =>
      reflect(And(anf(env,e1),anf(env,e2)))
    case Not(e) =>
      reflect(Not(anf(env,e)))
    case Cons(e1,e2) =>
      reflect(Cons(anf(env,e1),anf(env,e2)))
    case IsNum(e) =>
      reflect(IsNum(anf(env,e)))
    case IsStr(e) =>
      reflect(IsStr(anf(env,e)))
    case IsCons(e) =>
      reflect(IsCons(anf(env,e)))
    case IsCode(e) =>
      reflect(IsCode(anf(env,e)))
    case IsClosure(e) =>
      reflect(IsClosure(anf(env,e)))
    case Fst(e) =>
      reflect(Fst(anf(env,e)))
    case Snd(e) =>
      reflect(Snd(anf(env,e)))
    case Lift(e) =>
      reflect(Lift(anf(env,e)))
    case Run(b,e) =>
      reflect(Run(anf(env,b),reify(anf(env,e))))
    case Log(b,e) =>
      reflect(Log(anf(env,b),reify(anf(env,e))))
    case Special(f) =>
      reflect(Special(f))
  }


  def reifyc(f: => Val) = reify { val Code(e) = f; e }
  def reflectc(s: Exp) = Code(reflect(s))

  def reifyv(f: => Val) = run {
    stBlock = Nil
    val res = f
    if (stBlock != Nil) {
      // if we are generating code at all,
      // the result must be code
      val Code(last) = res
      Code((stBlock.map(_._2) foldRight last)(Let))
    } else {
      res
    }
  }

  // NBE-style 'reify' operator (semantics -> syntax)
  // lifting is shallow, i.e. 
  //   Rep[A]=>Rep[B]  ==> Rep[A=>B]
  //   (Rep[A],Rep[B]) ==> Rep[(A,B)]
  def lift(v: Val): Exp = v match {
    case Cst(n) => // number
      Lit(n)
    case Str(s) => // string
      Sym(s)
    case Tup(a,b) => (a, b) match {
      case (Code(u),Code(v)) => reflect(Cons(u,v))
    }
    case Clo(env2,e2) => // function
      // NOTE: We memoize functions here. This is not essential, and 
      // could be removed, yielding exactly the code shown in the paper.
      stFun collectFirst { case (n,`env2`,`e2`) => n } match {
        case Some(n) =>
          Var(n)
        case None =>
          stFun :+= (stFresh,env2,e2)
          reflect(Lam(reify{ val Code(r) = evalms(env2:+Code(fresh()):+Code(fresh()),e2); r }))
      }
    case Code(e) => reflect(Lift(e))
  }

  // multi-stage evaluation
  def evalms(env: Env, e: Exp, force_log: Boolean = false): Val = e match {
    case Lit(n) => Cst(n)
    case Sym(s) => Str(s)
    case Var(n) => env(n)
    case Lam(e) => Clo(env,e)
    case Let(e1,e2) => 
      val v1 = evalms(env,e1)
      evalms(env:+v1,e2)

    case Lift(e) =>
      Code(lift(evalms(env,e)))

    case Run(b,e) =>
      // first argument decides whether to generate
      // `run` statement or run code directly
      evalms(env,b) match {
        case Code(b1) =>
          reflectc(Run(b1, reifyc(evalms(env,e))))
        case _ =>
          val code = reifyc({ stFresh = env.length; evalms(env, e) })
          reifyv(evalms(env, code))
      }

    case Log(b,e) =>
      evalms(env,b) match {
        case Code(b1) =>
          reflectc(Log(b1, reifyc(evalms(env,e))))
        case _ =>
          val r = evalms(env, e)
          log(r)
          r
      }

    case App(e1,e2) =>
      (evalms(env,e1), evalms(env,e2)) match {
        case (Clo(env3,e3), v2) => 
          evalms(env3:+Clo(env3,e3):+v2,e3)
        case (Code(s1), Code(s2)) =>
          Code(reflect(App(s1,s2)))
        case (r1, r2) =>
          val r2s = r2 match {
            case Clo(_, _) => r2.getClass.toString
            case _ => r2.toString
          }
          throw new Exception(s"wrong app: ${r1.toString} $r2s")
      }

    case If(c,a,b) =>
      evalms(env,c) match {
        case Cst(n) => 
          if (n != 0) evalms(env,a) else evalms(env,b)
        case (Code(c1)) =>
          reflectc(If(c1, reifyc(evalms(env,a)), reifyc(evalms(env,b))))
      }

    case Plus(e1,e2) =>
      (evalms(env,e1), evalms(env,e2)) match {
        case (Cst(n1), Cst(n2)) =>
          Cst(n1+n2)
        case (Code(s1),Code(s2)) =>
          reflectc(Plus(s1,s2))
      }
    case Minus(e1,e2) =>
      (evalms(env,e1), evalms(env,e2)) match {
        case (Cst(n1), Cst(n2)) =>
          Cst(n1-n2)
        case (Code(s1),Code(s2)) =>
          reflectc(Minus(s1,s2))
      }
    case Times(e1,e2) =>
      (evalms(env,e1), evalms(env,e2)) match {
        case (Cst(n1), Cst(n2)) =>
          Cst(n1*n2)
        case (Code(s1),Code(s2)) =>
          reflectc(Times(s1,s2))
      }
    case Equ(e1,e2) =>
      (evalms(env,e1), evalms(env,e2)) match {
        case (v1, v2) if !v1.isInstanceOf[Code] && !v2.isInstanceOf[Code] =>
          Cst(if (v1 == v2) 1 else 0)
        case (Code(s1),Code(s2)) =>
          reflectc(Equ(s1,s2))
      }
    case Gt(e1,e2) =>
      (evalms(env,e1), evalms(env,e2)) match {
        case (Cst(n1), Cst(n2)) =>
          Cst(if (n1 > n2) 1 else 0)
        case (Code(s1),Code(s2)) =>
          reflectc(Gt(s1,s2))
      }
    case Lt(e1,e2) =>
      (evalms(env,e1), evalms(env,e2)) match {
        case (Cst(n1), Cst(n2)) =>
          Cst(if (n1 < n2) 1 else 0)
        case (Code(s1),Code(s2)) =>
          reflectc(Lt(s1,s2))
      }
    case Or(e1,e2) =>
      (evalms(env,e1), evalms(env,e2)) match {
        case (Cst(n1), Cst(n2)) =>
          Cst(if (n1 == 1 || n2 == 1) 1 else 0)
        case (Code(s1),Code(s2)) =>
          reflectc(Or(s1,s2))
      }
    case And(e1,e2) =>
      evalms(env,e1) match {
        case Cst(1) =>
          evalms(env,e2) match {
            case Cst(1) => Cst(1)
            case otherwise => Cst(0)
          }
        case Cst(0) => Cst(0)
        case Code(s1) =>
          evalms(env, e2) match {
            case Code(s2) => reflectc(And(s1,s2))
          }
      }
    case Not(e) =>
      evalms(env,e) match {
        case Cst(n) =>
          Cst(if (n == 1) 0 else 1)
        case Code(s) =>
          reflectc(Not(s))
      }
    case Cons(e1,e2) =>
      // introduction form, needs explicit lifting
      // (i.e. don't match on args)
      Tup(evalms(env,e1),evalms(env,e2))
    case Fst(e1) =>
      (evalms(env,e1)) match {
        case (Tup(a,b)) => 
          a
        case (Code(s1)) =>
          Code(reflect(Fst(s1)))
      }
    case Snd(e1) =>
      (evalms(env,e1)) match {
        case (Tup(a,b)) => 
          b
        case (Code(s1)) =>
          Code(reflect(Snd(s1)))
      }
    case IsNum(e1) =>
      (evalms(env,e1)) match {
        case (Code(s1)) =>
          Code(reflect(IsNum(s1)))
        case v => 
          Cst(if (v.isInstanceOf[Cst]) 1 else 0)
      }
    case IsStr(e1) =>
      (evalms(env,e1)) match {
        case (Code(s1)) =>
          Code(reflect(IsStr(s1)))
        case v => 
          Cst(if (v.isInstanceOf[Str]) 1 else 0)
      }

    case IsCons(e1) =>
      (evalms(env,e1)) match {
        case (Code(s1)) =>
          Code(reflect(IsCons(s1)))
        case v => 
          Cst(if (v.isInstanceOf[Tup]) 1 else 0)
      }

     case IsCode(e1) =>
      (evalms(env,e1)) match {
        case v => 
          Cst(if (v.isInstanceOf[Code]) 1 else 0)
      }

     case IsClosure(e1) =>
      (evalms(env,e1)) match {
        case (Code(s1)) =>
          Code(reflect(IsClosure(s1)))
        case v =>
          Cst(if (v.isInstanceOf[Clo]) 1 else 0)
      }

    // special forms: custom eval, ...
    case Special(f) => f(env)
  }

  // pretty printing
  var indent = "\n"
  def block(a: => String): String = {
    val save = indent
    indent += "  "
    try a finally indent = save
  }

  def pretty_aux(e: Exp, env: List[String]): String = e match {
    case Lit(n)     => n.toString
    case Sym(n)     => "'"+n
    case Var(x)     => try env(x) catch { case _ => "?" }
    case IsNum(a)   => s"(num? ${pretty_aux(a,env)})"
    case IsStr(a)   => s"(sym? ${pretty_aux(a,env)})"
    case Lift(a)    => s"(lift ${pretty_aux(a,env)})"
    case Cons(a, b)     => s"(cons ${pretty_aux(a,env)} ${pretty_aux(b,env)})"
    case And(a,b)     => s"(and ${pretty_aux(a,env)} ${pretty_aux(b,env)})"
    case Fst(a)     => s"(car ${pretty_aux(a,env)})"
    case Snd(a)     => s"(cdr ${pretty_aux(a,env)})"
    case Equ(a,b)   => s"(eq? ${pretty_aux(a,env)} ${pretty_aux(b,env)})"
    case Plus(a,b)  => s"(+ ${pretty_aux(a,env)} ${pretty_aux(b,env)})"
    case Minus(a,b) => s"(- ${pretty_aux(a,env)} ${pretty_aux(b,env)})"
    case Times(a,b) => s"(* ${pretty_aux(a,env)} ${pretty_aux(b,env)})"
    case Run(a,b)   => s"(run ${pretty_aux(a,env)} ${pretty_aux(b,env)})"
    case Log(a,b)   => s"(log ${pretty_aux(a,env)} ${pretty_aux(b,env)})"
    case App(a,b)   => s"(${pretty_aux(a,env)} ${pretty_aux(b,env)})"
    case Let(a,Var(n)) if n == env.length => pretty_aux(a,env)
    case Let(a,b)   => s"${indent}(let x${env.length} ${block(pretty_aux(a,env))} ${(pretty_aux(b,env:+("x"+env.length)))})"
    case Lam(e)     => s"${indent}(lambda f${env.length} x${env.length+1} ${block(pretty_aux(e,env:+("f"+env.length):+("x"+(env.length+1))))})"
    case If(c,a,b)  => s"${indent}(if ${pretty_aux(c,env)} ${block(pretty_aux(a,env))} ${indent}${block(pretty_aux(b,env))})"
    case _          => e.toString
  }

  def pretty(e: Exp, env: List[String], max_depth: Int = 30): String = {
    var ret = pretty_aux(e, env)
    val count = ret.count(_ == '\n')
    if(count >= max_depth)
      ret.split('\n').slice(0, max_depth).mkString("\n") + "\n<...>"
    else
      ret
  }

  var testsRun = 0
  def testDone(): Unit = {
    println(s"  Tests run: $testsRun"); testsRun = 0
  }

  def check(a:Any)(s:String) = if (a.toString.trim != s.trim) {
    println("error: expected ")
    println("    "+s)
    println("but got")
    println("    "+a)
    (new AssertionError).printStackTrace
  } else testsRun += 1


  // basic test cases
  def test() = {
    println("// ------- Base.test --------")
    println("Staged factorial...")
/*
  pattern:
    def f = fun { n => if (n != 0) f(n-1) else 1 }
  corresponds to:
    val f = { () => lift({ n => if (n != 0) f()(n-1) else 1 }) }
*/
    val f_self = App(Var(0),Lit(99))
    val n = Var(3)

    val fac_body = Lam(If(n,Times(n,App(f_self,Minus(n,Lift(Lit(1))))),Lift(Lit(1))))
    val fac = App(Lam(Lift(fac_body)),Lit(99))
    val code = reifyc(evalms(Nil,fac))
    val out = 
      Let(Lam(
        Let(If(Var(1),
              Let(Minus(Var(1),Lit(1)),
              Let(App(Var(0),Var(2)),
              Let(Times(Var(1),Var(3)),
              Var(4)))),
            /* else */
              Lit(1)
        ),
        Var(2))),
      Var(0))

    check(code)(out.toString)

    check(evalms(Nil,App(code,Lit(4))))("Cst(24)")

    testDone()
  }


}
