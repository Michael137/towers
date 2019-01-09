// multi-level core language λ↑↓* as a CESK machine in Scala
// Modified to support side-effects

import scala.collection.mutable.ListBuffer
import scala.collection.mutable.HashMap
import scala.util.hashing

object EBase {
  def log(x: Val): Unit = {
    println(s"LOGGED: $x");
  }

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
  case class Lt(a:Exp,b:Exp) extends Primitive
  case class IsNum(a:Exp) extends Primitive
  case class IsStr(a:Exp) extends Primitive
  case class IsCons(a:Exp) extends Primitive
  case class Fst(a:Exp) extends Primitive
  case class Snd(a:Exp) extends Primitive

  // Complex exp
  case class Cons(a:Exp,b:Exp) extends Exp
  case class If(c:Exp,a:Exp,b:Exp) extends Exp
  case class SetVar(v: Exp, e: Exp) extends Exp
  case class SetCar(v: Exp, e: Exp) extends Exp
  case class Log(b:Exp,e:Exp) extends Exp

  // Auxiliary
  case class Let(v: Var, e: Exp, body: Exp) extends Exp
  case class Letrec(p: List[(Var, Exp)], b: Exp) extends Exp
  case class App(f: Exp, arg: List[Exp]) extends Exp
  case class Lift(e:Exp) extends Exp
  case class Run(b:Exp,e:Exp) extends Exp

  // For mutation
  var cells = HashMap[String, List[Val]]()
  case class Cell(key: String, ptr: Int) extends Val
  // case class Tup_(a: Val, b: Val) extends Val
  case class ListRef(a:Exp) extends Primitive
  case class Ref(a:Exp) extends Primitive
  case class Cons_(a:Exp, b:Exp) extends Exp
  case class Fst_(a:Exp) extends Primitive
  case class Snd_(a:Exp) extends Primitive
  def deref(v: Val): Val = v match {
    case Cell(k, idx) => deref(cells(k)(idx))
    case _ => v
  }
  def listderef(v: Val): List[Val] = {
    v match {
      case c: Cell => listderef(cells(c.key)(0)):::listderef(cells(c.key)(1))
      case _ => v :: Nil
    }
  }
  def refToTuple(v: Val): Val = {
    v match {
      case c: Cell => Tup(refToTuple(cells(c.key)(0)),refToTuple(cells(c.key)(1)))
      case _ => v
    }
  }

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
  def gensym(v: String) = { s"$v${fresh()}" }

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

  // semantics -> syntax
  def lift(v: Val): Exp = v match {
    // TODO: make sure of semantics for cells here
    case c: Cell => lift(refToTuple(c))
    case Cst(n) => Lit(n)
    case Str(s) => Sym(s)
    case Tup(a,b) => (a,b) match {
      case (Code(u), Code(v)) => reflect(Cons(u,v)) // Add Cons to stBlock and return Var
      case (Code(u), t: Tup) => reflect(Cons(u,lift(t)))
    }
    case Code(e) => reflect(Lift(e))
  }

  // multi-stage evaluation
  def evalms(arg: Val): Val = {
    arg match {
      case st @ State(c: Exp, e: Env, s: Store, k: Cont) => {
        val ret = c match {
          case If(cond, conseq, alt) =>
            deref(evalms(State(cond, e, s, Halt())).asInstanceOf[Answer].v) match {
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
            val proc = evalAtom(f, e, s) // TODO: should be evalms
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

          case SetVar(v: Var, exp) =>
            val value = evalms(State(exp, e, s, Halt())).asInstanceOf[Answer].v
            val updated = update(s, e(v.s), value)
            val ret = applyCont(k, value, updated, e)
            // val ret = applyCont(k, null, updated, e)
            ret

          case SetCar(v: Var, exp) =>
            val value = inject(exp, e, s, false)
            inject(v, e, s, false) match {
              // case Cell(k, idx) => println(deref(Cell(k, 1)));cells(k) = List(value, cells(k)(1));println(deref(cells(k)(1)));
              case Cell(k, idx) => cells(k) = List(value, cells(k)(1))
            }
            val ret = applyCont(k, value, s, e)
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

          case Log(b,exp) =>
            inject(b, e, s, false) match {
              case Code(b1) =>
                reflectc(Log(b1, reifyc(inject(exp, e, s, false))))
              case _ =>
                val r = inject(exp, e, s, false)
                // log(r)
                println(s"LOGGED: $r")
                applyCont(k, r, s, e)
          }

          case Special(f) => applyCont(k, f(e), s, e)

          case _ if(isAtom(c)) => applyCont(k, evalAtom(c, e, s), s, e)
        }
        evalms(ret)
      }
      case Answer(_, _, _) => arg
    }
  }

  // Helper functions
  def isAtom(c: Exp) = c match {
    case Lit(_) | Sym(_) | Lam(_, _) | Cons(_, _) | Var(_) | Cons_(_, _) | _: Primitive => true
  }

  def evalAtom(c: Exp, e: Env, s: Store): Val = c match {
      case Sym(str) => Str(str)
      case Var(str) => s(e(str))
      case Lit(num) => Cst(num)
      case Cons(e1,e2) => 
        val ret1 = evalms(State(e1, e, s, Halt())).asInstanceOf[Answer].v
        val ret2 = evalms(State(e2, e, s, Halt())).asInstanceOf[Answer].v
        Tup(ret1, ret2)
      case Cons_(e1,e2) => 
        val ret1 = inject(e1, e, s, false)
        val ret2 = inject(e2, e, s, false)
        val key = gensym("cell")
        cells += (key -> List(ret1, ret2))
        Cell(key, 0)
      case Lam(vs, body) => Clo(Lam(vs, body), e)
      case Plus(e1, e2) =>
        val ret1 = deref(evalms(State(e1, e, s, Halt())).asInstanceOf[Answer].v)
        val ret2 = deref(evalms(State(e2, e, s, Halt())).asInstanceOf[Answer].v)
        (ret1, ret2) match {
          case (Cst(n1), Cst(n2)) => Cst(n1 + n2)
          case (Code(n1),Code(n2)) => reflectc(Plus(n1, n2))
          case _ => Str(s"Cannot perform + operation on expressions $ret1 and $ret2") // ? should be error instead
        }
      case Minus(e1, e2) =>
        val ret1 = deref(evalms(State(e1, e, s, Halt())).asInstanceOf[Answer].v)
        val ret2 = deref(evalms(State(e2, e, s, Halt())).asInstanceOf[Answer].v)
        (ret1, ret2) match {
          case (Cst(n1), Cst(n2)) => Cst(n1 - n2)
          case (Code(n1),Code(n2)) => reflectc(Minus(n1, n2))
          case _ => Str(s"Cannot perform - operation on expressions $e1 and $e2") // ? should be error instead
        }
      case Times(e1, e2) =>
        val ret1 = deref(evalms(State(e1, e, s, Halt())).asInstanceOf[Answer].v)
        val ret2 = deref(evalms(State(e2, e, s, Halt())).asInstanceOf[Answer].v)
        (ret1, ret2) match {
          case (Cst(n1), Cst(n2)) => Cst(n1 * n2)
          case (Code(n1),Code(n2)) => reflectc(Times(n1, n2))
          case _ => Str(s"Cannot perform * operation on expressions $e1 and $e2") // ? should be error instead
        }
      case Fst(e1) =>
        inject(e1, e, s, false) match {
          case Tup(a, b) => a
          case Code(a) => reflectc(Fst(a))
        }
      case Snd(e1) =>
        inject(e1, e, s, false) match {
          case Tup(a, b) => b
          case Code(a) => reflectc(Snd(a))
        }
        
      case Ref(e1) =>
        val lst = inject(e1, e, s, false)
        deref(lst)
        
      case ListRef(e1) =>
        val lst = inject(e1, e, s, false)
        refToTuple(lst)

      case Fst_(e1) =>
        inject(e1, e, s, false) match {
          case Cell(k, idx) => cells(k)(idx) match {
                                  case Cell(k2, idx2) => Cell(k2, 0)
                                  case _ => Cell(k, 0)
                                }
          case Code(a) => reflectc(Fst_(a))
          case Tup(a, b) => a
        }
      case Snd_(e1) =>
        inject(e1, e, s, false) match {
          case Cell(k, idx) => cells(k)(scala.math.min(1, idx + 1)) match {
                                  case Cell(k2, idx2) => Cell(k2, 0)
                                  case _ => Cell(k, scala.math.min(1, idx + 1))
                                }
          case Code(a) => reflectc(Snd_(a))
          case Tup(a, b) => b // TODO: Tup should be handled in lisp-frontend and not here. This is
                              //       curently for '(...) to work
        }
        
      case Equ(e1, e2) =>
        val ret1 = evalms(State(e1, e, s, Halt())).asInstanceOf[Answer].v
        val ret2 = evalms(State(e2, e, s, Halt())).asInstanceOf[Answer].v
        
        (ret1, ret2) match {
          case (v1, v2) if !v1.isInstanceOf[Code] && !v2.isInstanceOf[Code] => Cst(if (v1 == v2) 1 else 0)
          case (Code(n1),Code(n2)) => reflectc(Equ(n1, n2))
          case _ => Str(s"Cannot perform == operation on expressions $ret1 and $ret2") // ? should be error instead
        }

      case Lt(e1,e2) =>
        val ret1 = deref(evalms(State(e1, e, s, Halt())).asInstanceOf[Answer].v)
        val ret2 = deref(evalms(State(e2, e, s, Halt())).asInstanceOf[Answer].v)
        (ret1, ret2) match {
          case (Cst(n1), Cst(n2)) =>
            Cst(if (n1 < n2) 1 else 0)
          case (Code(s1),Code(s2)) =>
            reflectc(Gt(s1,s2))
          case _ => Str(s"Cannot perform < operation on expressions $ret1 and $ret2") // ? should be error instead
        }

      case Gt(e1,e2) =>
        val ret1 = evalms(State(e1, e, s, Halt())).asInstanceOf[Answer].v
        val ret2 = evalms(State(e2, e, s, Halt())).asInstanceOf[Answer].v
        (ret1, ret2) match {
          case (Cst(n1), Cst(n2)) =>
            Cst(if (n1 > n2) 1 else 0)
          case (Code(s1),Code(s2)) =>
            reflectc(Gt(s1,s2))
          case _ => Str(s"Cannot perform > operation on expressions $ret1 and $ret2") // ? should be error instead
        }
    }

  def update[A,B](store: StoreFun[A, B], a: A, b: B): StoreFun[A, B] = {
    x: A =>
      if(x == a)
        b
      else {
        // println(s"FROM UPDATE: $a")
        store(x)
      }
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

  def inject(e: Exp, env: Env = initEnv, store: Store = initStore, reset: Boolean = true) = {
    if(reset) {
      stFresh = 0
      stBlock = Nil
      cells = HashMap[String, List[Val]]()
    }

    evalms(State(e, env, store, Halt())).asInstanceOf[Answer].v
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
    runCellTests()

    TestHelpers.testDone()
  }
}