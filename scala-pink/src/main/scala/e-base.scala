// multi-level core language λ↑↓ as a definitional interpreter in Scala
// Modified to support side-effects

import scala.collection.mutable.ListBuffer
import scala.collection.mutable.HashMap

object EBase {
  var log: Val => Unit = {x => println(x)}

  // expressions
  abstract class Exp
  abstract class Primitive extends Exp
  // Atomic exp
  case class Lit(n:Int) extends Exp
  case class Sym(s:String) extends Exp
  case class Var(n:Int) extends Exp
  case class Lam(e:Exp) extends Exp
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
  case class SetVar(v:Var, e: Exp)
  case class Log(b:Exp,e:Exp) extends Exp

  // Auxiliary
  case class Let(e1:Exp,e2:Exp) extends Exp
  case class App(e1:Exp, e2:Exp) extends Exp
  case class Lift(e:Exp) extends Exp
  case class Run(b:Exp,e:Exp) extends Exp

  // for custom extensions to AST
  case class Special(f:Env => Val) extends Exp {
    override def toString = "<special>"
  }

  // values
  type Env = ListBuffer[Int] // de Bruijn Level => Addr
  type Store = HashMap[Int, Val] // Addr => Val
  type Cont = Val // Halt | Letk

  abstract class Val
  case class Cst(n:Int) extends Val
  case class Str(s:String) extends Val
  case class Clo(env:Env,e:Exp) extends Val
  case class Tup(v1:Val,v2:Val) extends Val
  case class Code(e:Exp) extends Val
  // Continuation types
  case class Letk(c: Exp, e: Env, v: Int, k: Cont) extends Val// var, v, is de Bruijn Level
  case class Halt() extends Val

  case class State(c: Exp, e: Env, s: Store, k: Cont) extends Val

  // interpreter state and mechanics
  var stFresh = 0
  var stBlock: ListBuffer[Exp] = new ListBuffer[Exp]
  var stFun: ListBuffer[(Int,Env,Exp)] = new ListBuffer[(Int,Env,Exp)]
  def run[A](f: => A): A = {
    val sF = stFresh
    val sB = stBlock
    val sN = stFun
    try { val ret = f; ret } finally { stFresh = sF; stBlock = sB; stFun = sN }
  }

  def fresh() = {
    stFresh += 1; Var(stFresh-1)
  }
  def reify(f: => Exp) = {
      run {
        stBlock = new ListBuffer[Exp]
        val last = f
        (stBlock foldRight last)(Let)
      }
  }
  def reflect(s:Exp) = {
    stBlock :+= s
    fresh()
  }

  def reifyc(f: => Val) = reify {
    f match {
      case Code(e) => e
      case Tup(Code(e1), Code(e2)) => Cons(e1, e2) // TODO: double check this behaviour is sound w.r.t reify semantics
    }
  }
  def reflectc(s: Exp) = Code(reflect(s))

  def reifyv(f: => Val) = run {
    stBlock = new ListBuffer[Exp]
    val res = f
    if (!stBlock.isEmpty) {
      // if we are generating code at all,
      // the result must be code
      val Code(last) = res
      Code((stBlock foldRight last)(Let))
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
    case Tup(a,b) =>
      val (Code(u),Code(v)) = (a,b)
      reflect(Cons(u,v))
    case Code(e) => reflect(Lift(e))
  }

  // multi-stage evaluation
  // import scala.annotation.tailrec
  // @tailrec
  def evalms(state: State): State = state match {
    case st @ State(c: Exp, e: Env, s: Store, k: Cont) => c match {
      case _ if(isAtom(c)) => applyCont(k, evalAtom(c, e, s), s)
      case If(cond, conseq, alt) =>
        evalAtom(cond, e, s) match {
          case Cst(b) => if(b != 0) State(conseq, e, s, k) else State(alt, e, s, k)
        }
      // case Let(exp, body) => State(exp, e, s, Letk(body, e, v, k))
    }
  }

  // Helper functions
  def isAtom(c: Exp) = c match {
    case Lit(_) | Sym(_) | Lam(_) | _: Primitive => true
  }

  def evalAtom(c: Exp, e: Env, s: Store): Val = c match {
      case Sym(str) => Str(str)
      case Lit(num) => Cst(num)
      case Var(num) => s(e(num))
      case Lam(lam) => Clo(e, lam)
      case Plus(e1, e2) =>
        (evalAtom(e1, e, s), evalAtom(e2, e, s)) match {
          case (Cst(n1), Cst(n2)) => Cst(n1 + n2)
        }
    }
  
  def applyCont(k: Cont, v: Val, s: Store): State = k match {
    case Halt() => State(null, null, s, k) // Answer
  }

  // basic test cases
  def test() = {
    println("// ------- EBase.test --------")
    // val f_self = App(Var(0),Lit(99))
    // val n = Var(3)
    // val fac_body = Lam(If(n,Times(n,App(f_self,Minus(n,Lift(Lit(1))))),Lift(Lit(1))))
    // val fac = App(Lam(Lift(fac_body)),Lit(99))
    // val code = reifyc(evalms(new ListBuffer[Int],fac,new HashMap[Int, Val]))
    // println(code)

    // Trivial set! example
    // (let x 2
    //   (let y (set! x 3)
    //      (+ x y)))
    // => 6
  }
}
