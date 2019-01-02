// multi-level core language λ↑↓* as a CESK machine in Scala
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
  case class Tup(v1:Val,v2:Val) extends Val
  // Continuation types
  case class LetK(v: Var, c: Exp, e: Env, k: Cont) extends Val // ? var, v, is de Bruijn Level
  case class Halt() extends Val

  case class Answer(v: Val, s: Store, e: Env) extends Val
  case class State(c: Exp, e: Env, s: Store, k: Cont) extends Val

  case class Code(e:Exp) extends Val

  // Staging operations
  var stBlock: List[(String, Exp)] = Nil
  def run[A](f: => A): A = {
    val sF = stFresh
    val sB = stBlock
    try { f } finally { stFresh = sF; stBlock = sB }
  }

  var stFresh = 0
  def fresh() = {
    stFresh += 1; stFresh
  }
  def gensym() = { s"x${fresh()}" }

  def reify(f: => Exp) = {
      run {
        stBlock = Nil
        val last = f
        stBlock.foldRight(last)({ (t: Tuple2[String,Exp], b: Exp) =>
                                      val (v, e) = t
                                      Let(Var(v), e, b) })
      }
  }
  def reflect(s:Exp) = {
    var varName = gensym()
    stBlock :+= (varName, s)
    Var(varName)
  }

  def reifyc(f: => Val) = reify {
    f match {
      case Code(e) => e
    }
  }
  def reflectc(s: Exp) = Code(reflect(s))

  def reifyv(f: => Val) = run {
    stBlock = Nil
    val res = f
    if (stBlock != Nil) {
      // if we are generating code at all,
      // the result must be code
      val Code(last) = res
      Code(
        stBlock.foldRight(last)({ (t: Tuple2[String,Exp], b: Exp) =>
                                      val (v, e) = t
                                      Let(Var(v), e, b) })
      )
    } else {
      res
    }
  }

  def lift(v: Val): Exp = v match {
    case Cst(n) => Lit(n)
    case Str(s) => Sym(s)
    case Tup(a,b) =>
      val (Code(u),Code(v)) = (a,b)
      reflect(Cons(u,v)) // Add to Cons to stBlock and return Var(stFresh)
    case Code(e) => reflect(Lift(e))
  }

  // multi-stage evaluation
  def evalms(arg: Val): Val = {
    arg match {
      case st @ State(c: Exp, e: Env, s: Store, k: Cont) => {
        val ret = c match {
          case If(cond, conseq, alt) =>
            evalms(State(cond, e, s, Halt())).asInstanceOf[Answer].v match {
              case Cst(b) => if(b != 0) State(conseq, e, s, k) else State(alt, e, s, k)
              case Code(c) =>
                applyCont(k, reflectc(If(c,
                                         reifyc(evalms(State(conseq, e, s, Halt())).asInstanceOf[Answer].v),
                                         reifyc(evalms(State(alt, e, s, Halt())).asInstanceOf[Answer].v))), null, e)
            }
          case Let(v, exp, body) => State(exp, e, s, LetK(v, body, e, k))

          // Function application
          case App(f, es) =>
            // NB: staging decision done in ``applyProc''
            val proc = evalAtom(f, e, s)
            val args = es.map({ x =>
                                evalms(State(x, e, s, Halt())).asInstanceOf[Answer].v
                              })
            applyProc(proc, args, s, k)

          case Letrec(exps, body) => // Letrec(List((v1, e1), (v2, e2) ..., (vn, en)), body)
            val (vs, es) = exps.unzip
            // val addrs = vs.map({ x: Var => x.s.hashCode() })
            val addrs = vs.map({ _ => fresh() })
            val varNames = vs.map({x: Var => x.s})
            val updatedEnv = updateMany(e, varNames, addrs)
            val vals = es.map({ x => evalAtom(x, updatedEnv, s) })
            val updatedStore = updateMany(s, addrs, vals)
            State(body, updatedEnv, updatedStore, k)

          case SetVar(v, exp) =>
            val value = evalms(State(exp, e, s, k)).asInstanceOf[Answer].v
            val updated = update(s, e(v.s), value)
            val ret = applyCont(k, null, updated, e)
            ret

          case Lift(exp) =>
            val trans = State(exp, e, s, Halt())
            val lifted = lift(evalms(trans).asInstanceOf[Answer].v)
            applyCont(k, Code(lifted), s, e)

          case Run(b,exp) =>
            // first argument decides whether to generate
            // `run` statement or run code directly
            evalms(State(b, e, s, Halt())).asInstanceOf[Answer].v match {
              case Code(b1) =>
                applyCont(k, 
                          reflectc(Run(b1, reifyc(evalms(State(exp, e, s, Halt())).asInstanceOf[Answer].v))),
                          s, e)
              case _ =>
                // TODO: verify stFresh reset not needed
                val code = reifyc({ /*stFresh = env.length;*/ evalms(State(exp, e, s, Halt())).asInstanceOf[Answer].v })
                applyCont(k,
                          reifyv(evalms(State(code, e, s, Halt())).asInstanceOf[Answer].v),
                          s, e)
            }

          case _ if(isAtom(c)) => applyCont(k, evalAtom(c, e, s), s, e)
        }
        evalms(ret)
      }
      case Answer(_, _, _) => arg
    }
  }

  // Helper functions
  def isAtom(c: Exp) = c match {
    case Lit(_) | Sym(_) | Lam(_, _) | Cons(_, _) | Var(_) | _: Primitive => true
  }

  def evalAtom(c: Exp, e: Env, s: Store): Val = c match {
      case Sym(str) => Str(str)
      case Var(str) => s(e(str))
      case Lit(num) => Cst(num)
      case Cons(e1,e2) => 
        val ret1 = evalms(State(e1, e, s, Halt())).asInstanceOf[Answer].v
        val ret2 = evalms(State(e2, e, s, Halt())).asInstanceOf[Answer].v
        Tup(ret1, ret2)
      case Lam(vs, body) => Clo(Lam(vs, body), e)
      case Plus(e1, e2) =>
        val ret1 = evalms(State(e1, e, s, Halt())).asInstanceOf[Answer].v
        val ret2 = evalms(State(e2, e, s, Halt())).asInstanceOf[Answer].v
        (ret1, ret2) match {
          case (Cst(n1), Cst(n2)) => Cst(n1 + n2)
          case (Code(n1),Code(n2)) => reflectc(Plus(n1, n2))
          case _ => Str(s"Cannot perform + operation on expressions $ret1 and $ret2") // ? should be error instead
        }
      case Minus(e1, e2) =>
        val ret1 = evalms(State(e1, e, s, Halt())).asInstanceOf[Answer].v
        val ret2 = evalms(State(e2, e, s, Halt())).asInstanceOf[Answer].v
        (ret1, ret2) match {
          case (Cst(n1), Cst(n2)) => Cst(n1 - n2)
          case (Code(n1),Code(n2)) => reflectc(Plus(n1, n2))
          case _ => Str(s"Cannot perform - operation on expressions $e1 and $e2") // ? should be error instead
        }
      case Times(e1, e2) =>
        val ret1 = evalms(State(e1, e, s, Halt())).asInstanceOf[Answer].v
        val ret2 = evalms(State(e2, e, s, Halt())).asInstanceOf[Answer].v
        (ret1, ret2) match {
          case (Cst(n1), Cst(n2)) => Cst(n1 * n2)
          case (Code(n1),Code(n2)) => reflectc(Plus(n1, n2))
          case _ => Str(s"Cannot perform * operation on expressions $e1 and $e2") // ? should be error instead
        }
      case Fst(e1) =>
        val Tup(a, b) = evalAtom(e1, e, s)
        a
      case Snd(e1) =>
        val Tup(a, b) = evalAtom(e1, e, s)
        b
      case Equ(e1, e2) =>
        val ret1 = evalms(State(e1, e, s, Halt())).asInstanceOf[Answer].v
        val ret2 = evalms(State(e2, e, s, Halt())).asInstanceOf[Answer].v
        (ret1, ret2) match {
          case (Cst(n1), Cst(n2)) => if(n1 == n2) Cst(1) else Cst(0)
          case (Code(n1),Code(n2)) => reflectc(Equ(n1, n2))
          case _ => Str(s"Cannot perform == operation on expressions $ret1 and $ret2") // ? should be error instead
        }
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

  def applyCont(k: Cont, v: Val, s: Store, e: Env): Val = k match {
    case Halt() => Answer(v, s, e) // Answer
    case LetK(Var(vr), c, e, k) =>
      // val addr = vr.hashCode()
      val addr = fresh()
      val updatedEnv = update(e, vr, addr)
      val updatedStore = update(s, addr, v)
      State(c, updatedEnv, updatedStore, k)
  }

  def applyProc(proc: Val, args: List[Val], s: Store, k: Cont) = proc match {
    case Clo(Lam(vs, body), env) =>
      // val addrs = vs.map({x: Var => x.s.hashCode()})
      val addrs = vs.map({_ => fresh()})
      val varNames = vs.map({x: Var => x.s})
      val updatedEnv = updateMany(env, varNames, addrs)
      val updatedStore = updateMany(s, addrs, args)
      State(body, updatedEnv, updatedStore, k)

    // TODO: check if all args are Code as well?
    case Code(s1) => 
      val codeArgs = args.map({ a => val Code(res) = a; res })
      applyCont(k, reflectc(App(s1, codeArgs)), s, null)
  }

  // TODO: should be more robust
  val initEnv = {arg: String => -1}
  val initStore = {arg: Int => Str("Error: using init store")}

  def inject(e: Exp) = {
    stFresh = 0
    stBlock = Nil

    evalms(State(e, initEnv, initStore, Halt())).asInstanceOf[Answer].v
  }

  /*****************
  ***** TESTS ******
  ******************/
  def test() = {
    import EBaseTests._

    println("// ------- EBase.test --------")

    expressionTests()
    letrecTests()
    factorialTests()
    liftTests()
    runExpTests()

    TestHelpers.testDone()
  }
}