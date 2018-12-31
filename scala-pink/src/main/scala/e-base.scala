// multi-level core language λ↑↓ as a definitional interpreter in Scala
// Modified to support side-effects

import scala.collection.mutable.ListBuffer
import scala.collection.mutable.HashMap
import scala.util.hashing

object EBase {
  var log: Val => Unit = {x => println(x)}

  // expressions
  abstract class Exp
  abstract class Primitive extends Exp
  // Atomic exp
  case class Lit(n:Int) extends Exp
  case class Sym(s:String) extends Exp
  case class Var(s:String) extends Exp
  case class Lam(vs: List[Var], e:Exp) extends Exp
  // Primitives
  case class Plus(a:Exp,b:Exp) extends Primitive
  case class Minus(a:Exp,b:Exp) extends Primitive
  case class Times(a:Exp,b:Exp) extends Primitive
  case class Equ(a:Exp,b:Exp) extends Primitive
  case class Gt(a:Exp,b:Exp) extends Primitive
  case class IsNum(a:Exp) extends Primitive
  case class IsStr(a:Exp) extends Primitive
  case class IsCons(a:Exp) extends Primitive
  case class Fst(a:Exp) extends Primitive
  case class Snd(a:Exp) extends Primitive

  // Complex exp
  case class Cons(a:Exp,b:Exp) extends Exp
  case class If(c:Exp,a:Exp,b:Exp) extends Exp
  case class SetVar(v: Var, e: Exp) extends Exp
  case class Log(b:Exp,e:Exp) extends Exp

  // Auxiliary
  case class Let(v: Var, e: Exp, body: Exp) extends Exp
  case class Letrec(p: List[(Var, Exp)], b: Exp) extends Exp
  case class App(f: Exp, arg: List[Exp]) extends Exp
  case class Lift(e:Exp) extends Exp
  case class Run(b:Exp,e:Exp) extends Exp

  // for custom extensions to AST
  case class Special(f:Env => Val) extends Exp {
    override def toString = "<special>"
  }

  // values
  type StoreFun[A, B] = (A => B)
  type Env = StoreFun[String, Int] // Variable => Addr // ? should be [Var, Int]
  type Store = StoreFun[Int, Val] // Addr => Val
  type Cont = Val // Halt | Letk

  abstract class Val
  case class Cst(n:Int) extends Val
  case class Str(s:String) extends Val
  case class Clo(f: Lam, env: Env) extends Val
  case class Code(e:Exp) extends Val
  case class Tup(v1:Val,v2:Val) extends Val
  // Continuation types
  case class LetK(v: Var, c: Exp, e: Env, k: Cont) extends Val // ? var, v, is de Bruijn Level
  case class Halt() extends Val

  case class Answer(v: Val, s: Store) extends Val
  case class State(c: Exp, e: Env, s: Store, k: Cont) extends Val

  val initEnv = {arg: String => -1}
  val initStore = {arg: Int => Str("Error: using init store")}

  // multi-stage evaluation
  def evalms(arg: Val): Val = {
    arg match {
      case st @ State(c: Exp, e: Env, s: Store, k: Cont) => {
        val ret = c match {
          case If(cond, conseq, alt) =>
            evalAtom(cond, e, s) match {
              case Cst(b) => if(b != 0) State(conseq, e, s, k) else State(alt, e, s, k)
            }
          case Let(v, exp, body) => State(exp, e, s, LetK(v, body, e, k))

          // Function application
          case App(f, es) =>
            val proc = evalAtom(f, e, s)
            val args = es.map({x=>evalAtom(x,e,s)})
            applyProc(proc, args, s, k)

          case Letrec(exps, body) => // Letrec(List((v1, e1), (v2, e2) ..., (vn, en)), body)
            val (vs, es) = exps.unzip
            val addrs = vs.map({ x: Var => x.s.hashCode() })
            val varNames = vs.map({x: Var => x.s})
            val updatedEnv = updateMany(e, varNames, addrs)
            val vals = es.map({ x => evalAtom(x, updatedEnv, s) })
            val updatedStore = updateMany(s, addrs, vals)
            State(body, updatedEnv, updatedStore, k)

          case SetVar(v, exp) =>
            val value = evalAtom(exp, e, s)
            val updated = update(s, e(v.s), value)
            applyCont(k, null, updated)
          case _ if(isAtom(c)) => applyCont(k, evalAtom(c, e, s), s)
        }
        evalms(ret)
      }
      case Answer(_, _) => arg
    }
  }

  // Helper functions
  def isAtom(c: Exp) = c match {
    case Lit(_) | Sym(_) | Lam(_, _) | Cons(_, _) | _: Primitive => true
  }

  def evalAtom(c: Exp, e: Env, s: Store): Val = c match {
      case Sym(str) => Str(str)
      case Var(str) => s(e(str))
      case Lit(num) => Cst(num)
      case Cons(e1,e2) => Tup(evalAtom(e1, e, s), evalAtom(e2, e, s))
      case Lam(vs, body) => Clo(Lam(vs, body), e)
      case Plus(e1, e2) =>
        (evalAtom(e1, e, s), evalAtom(e2, e, s)) match {
          case (Cst(n1), Cst(n2)) => Cst(n1 + n2)
          case _ => Str(s"Cannot perform + operation on expressions $e1 and $e2") // ? should be error instead
        }
      case Minus(e1, e2) =>
        (evalAtom(e1, e, s), evalAtom(e2, e, s)) match {
          case (Cst(n1), Cst(n2)) => Cst(n1 - n2)
          case _ => Str(s"Cannot perform - operation on expressions $e1 and $e2") // ? should be error instead
        }
      case Times(e1, e2) =>
        (evalAtom(e1, e, s), evalAtom(e2, e, s)) match {
          case (Cst(n1), Cst(n2)) => Cst(n1 * n2)
          case _ => Str(s"Cannot perform * operation on expressions $e1 and $e2") // ? should be error instead
        }
      case Fst(e1) =>
        val Tup(a, b) = evalAtom(e1, e, s)
        a
      case Snd(e1) =>
        val Tup(a, b) = evalAtom(e1, e, s)
        b
    }

  def update[A,B](store: StoreFun[A, B], a: A, b: B): StoreFun[A, B] = {
    x: A =>
      if(x == a)
        b
      else
        store(x)
  }

  def updateMany[A, B](store: StoreFun[A, B], as: List[A], bs: List[B]): StoreFun[A, B] = (as, bs) match {
    case (a::tla, b::tlb) => updateMany(update(store, a, b), tla, tlb)
    case (Nil, Nil) | _ => store
  }
  
  def applyCont(k: Cont, v: Val, s: Store): Val = k match {
    case Halt() => Answer(v, s) // Answer
    case LetK(Var(vr), c, e, k) =>
      val addr = vr.hashCode()
      State(c, update(e, vr, addr), update(s, addr, v), k)
  }

  def applyProc(proc: Val, args: List[Val], s: Store, k: Cont) = proc match {
    case Clo(Lam(vs, body), env) =>
      val addrs = vs.map({x: Var => x.s.hashCode()})
      val varNames = vs.map({x: Var => x.s})
      val updatedEnv = updateMany(env, varNames, addrs)
      val updatedStore = updateMany(s, addrs, args)
      State(body, updatedEnv, updatedStore, k)
  }

  // basic test cases
  def test() = {
    println("// ------- EBase.test --------")

    val exp = Plus(Lit(2), Lit(2))
    println(evalms(State(exp, initEnv, initStore, Halt())))

    val exp1 = SetVar(Var("x"), Plus(Var("x"), Var("y")))
    val exp2 = Let(Var("x"), Lit(136), exp1)
    val exp3 = Let(Var("y"), Lit(1), exp2)
    val ret = evalms(State(exp3, initEnv, initStore, Halt())).asInstanceOf[Answer]
    println(ret.s("x".hashCode()))

    // x = 136; (\x -> x + y) x
    val funExp = Let(Var("x"), Lit(136), App(
                                Lam(List(Var("y")), // Fun
                                    Plus(Var("x"), Var("y"))),
                                List(Var("x")))) // Args
    println(evalms(State(funExp, initEnv, initStore, Halt())).asInstanceOf[Answer].v)

    val letrecExp = Letrec(
                          List((Var("x"), Lit(136)), (Var("y"), Lit(1))),
                          Plus(Var("x"), Var("y"))
                        )
    println(evalms(State(letrecExp, initEnv, initStore, Halt())).asInstanceOf[Answer].v)

    val consExp = Let(Var("lst"), Cons(Lit(2), Cons(Lit(3), Lit(4))), Snd(Var("lst")))
    println(evalms(State(consExp, initEnv, initStore, Halt())).asInstanceOf[Answer].v)

    // set-car!
    val setCarExp = Let(Var("lst"), Cons(Lit(1), Cons(Lit(2), Cons(Lit(3), Lit(4)))),
                        SetVar(Var("lst"), Cons(Lit(0), Snd(Var("lst")))))
    val res = evalms(State(setCarExp, initEnv, initStore, Halt())).asInstanceOf[Answer].s
    println(res("lst".hashCode()))

    // set-cdr!
    val setCdrExp = Let(Var("lst"), Cons(Lit(1), Cons(Lit(2), Cons(Lit(3), Lit(4)))),
                        SetVar(Var("lst"), Cons(Fst(Var("lst")), Lit(0))))
    val res2 = evalms(State(setCdrExp, initEnv, initStore, Halt())).asInstanceOf[Answer].s
    println(res2("lst".hashCode()))

    // factorial
    // val facExp = 
    //   Letrec(List((Var("n"), Lit(6)),
    //               (Var("f"), Lam(List(Var("x")),
    //                                 If(Var("x"),
    //                                   Times(Var("x"), App(Var("f"), List(Minus(Var("x"), Lit(1))))),
    //                                   Lit(1))))),
    //               App(Var("f"), List(Var("n"))))
    // println(evalms(State(facExp, initEnv, initStore, Halt())).asInstanceOf[Answer].v)


    // Letrec: Simple usage
    val letrecTestExps1 = Letrec(List((Var("f"), Lam(List(Var("ctr")), If(Var("ctr"),
                                                                Plus(Lit(5), Lit(5)),
                                                                Plus(Lit(-5), Lit(-5)))))),
                        App(Var("f"), List(Lit(1))))
    println(evalms(State(letrecTestExps1, initEnv, initStore, Halt())).asInstanceOf[Answer].v)

    // Letrec: Recursive call
    val letrecTestExps2 = Letrec(List((Var("f"), Lam(List(Var("ctr")), If(Var("ctr"),
                                                            App(Var("f"), List(Minus(Var("ctr"), Lit(1)))),
                                                            Plus(Lit(-5), Lit(-5)))))),
                    App(Var("f"), List(Lit(10000))))
    println(evalms(State(letrecTestExps2, initEnv, initStore, Halt())).asInstanceOf[Answer].v)

    // ! Letrec: Recursive call within primitive operation
    val letrecTestExps3 = Letrec(List((Var("f"), Lam(List(Var("ctr")), If(Var("ctr"),
                                                            App(Var("f"), List(Minus(Var("ctr"), Lit(1)))),
                                                            Plus(Lit(-5), Lit(-5)))))),
                    App(Var("f"), List(Lit(10000))))
    println(evalms(State(letrecTestExps3, initEnv, initStore, Halt())).asInstanceOf[Answer].v)
  }
}
