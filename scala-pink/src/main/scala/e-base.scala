// multi-level core language λ↑↓* as a CESK machine in Scala
// Modified to support side-effects

import scala.collection.mutable.ListBuffer
import scala.collection.mutable.HashMap
import scala.collection.mutable.LinkedHashMap
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
  case class VarargLam(e: Exp, vs: Exp) extends Exp
  // Primitives
  case class Plus(a:Exp,b:Exp) extends Primitive
  case class Minus(a:Exp,b:Exp) extends Primitive
  case class Times(a:Exp,b:Exp) extends Primitive
  case class Equ(a:Exp,b:Exp) extends Primitive
  case class And(a:Exp,b:Exp) extends Primitive
  case class Gt(a:Exp,b:Exp) extends Primitive
  case class Lt(a:Exp,b:Exp) extends Primitive
  case class IsNum(a:Exp) extends Primitive
  case class IsStr(a:Exp) extends Primitive
  case class IsCons(a:Exp) extends Primitive
  case class IsCell(a:Exp) extends Primitive
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
  case class VarargLet(e: Exp, vs: Exp) extends Exp
  case class App(f: Exp, arg: List[Exp]) extends Exp
  case class Lift(e:Exp) extends Exp
  case class Run(b:Exp,e:Exp) extends Exp

  // For mutation
  var cells = LinkedHashMap[String, List[Val]]()
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
  case class Clo(f: Lam, state: State) extends Val
  case class Tup(v1:Val,v2:Val) extends Val
  // Continuation types
  case class LetK(v: Var, c: Exp, e: Env, k: Cont) extends Val // ? var, v, is de Bruijn Level
  case class Halt() extends Val

  case class Answer(v: Val, s: Store, e: Env) extends Val
  case class State(c: Exp, e: Env, s: Store, k: Cont) extends Val
  case class Null() extends Val

  case class Code(e:Exp) extends Val

  // Staging operations
  var stBlock: LinkedHashMap[String, Exp] = LinkedHashMap.empty
  def run[A](f: => A): A = {
    val sF = stFresh
    val sB = stBlock
    try { f } finally { stFresh = sF; stBlock = sB }
  }

  var stFresh = 0
  def fresh(): Int = {
    stFresh += 1; stFresh
  }
  def gensym() = { s"x${fresh()}" }
  def gensym(v: String) = { s"$v${fresh()}" }

  def reify(f: => Exp) = {
      run {
        stBlock = LinkedHashMap.empty
        val last = f
        val ret = stBlock.foldRight(last)({ (t: Tuple2[String,Exp], b: Exp) =>
                                      val (v, e) = t
                                      Let(Var(v), e, b) })
        ret
      }
  }
  def reflect(s:Exp) = {
    var varName = gensym()
    stBlock += varName -> s
    Var(varName)
  }

  def reifyc(f: => Val) = reify {
    deref(f) match {
      case Code(e) => e
    }
  }
  def reflectc(s: Exp) = Code(reflect(s))

  def reifyv(f: => Val) = run {
    stBlock = LinkedHashMap.empty
    val res = f
    if (stBlock != LinkedHashMap.empty) {
      // if we are generating code at all,
      // the result must be code
      res match {
        case Code(last) =>
          Code(
            stBlock.foldRight(last)({ (t: Tuple2[String,Exp], b: Exp) =>
                                          val (v, e) = t
                                          Let(Var(v), e, b) })
          )
        case other => other
      }
    } else {
      res
    }
  }

  // semantics -> syntax
  def lift(v: Val): Exp = v match {
    // TODO: make sure of semantics for cells here
    case c: Cell => val tup = refToTuple(c); /*println(tup);*/ lift(tup)
    case Cst(n) => Lit(n)
    case Str(s) => Sym(s)
    case Tup(a,b) => (a,b) match {
      case (Code(u), Code(v)) => reflect(Cons(u,v)) // Add Cons to stBlock and return Var
      case (Code(u), t: Tup) => reflect(Cons(u,lift(t)))
      case (u, t) => reflect(Cons(lift(u),lift(t)))
    }
    case Code(e) => reflect(Lift(e))
    case Clo(Lam(vs: List[Var], f), state) =>
      // TODO: put back memoization?
      val missingVars = vs.map({ v: Var => if(state.s(state.e(v.s)) == Str(initStoreErrorStr)) v })
      if(missingVars.size > 0) {
        val addrs = missingVars.asInstanceOf[List[Var]].map({_: Var => fresh()})
        val varNames = missingVars.asInstanceOf[List[Var]].map({v: Var => v.s})
        val args = varNames.map({s: String => Code(Var(s))})
        val updatedEnv = updateMany(state.e, varNames, addrs)
        val updatedStore = updateMany(state.s, addrs, args)
        reflect(Lam(vs, reify{ val Code(r) = deref(inject(f, updatedEnv, updatedStore, false)); r }))
      } else {
        val Code(r) = inject(f, state.e, state.s, false)
        reflect(Lam(vs, reify{ val Code(r) = inject(f, state.e, initStore, false); r }))
      }
  }

  var recursionDepth = 0
  // multi-stage evaluation
  def evalms(arg: Val): Val = {
    arg match {
      case st @ State(c: Exp, e: Env, s: Store, k: Cont) => {
        recursionDepth += 1;
        val ret = c match {
          case If(cond, conseq, alt) =>
            val evaled = deref(inject(cond, e, s, false))
            // println("IF: " + cond + " ====> " + evaled)
            // println("CONSEQ: " + conseq + " ALT: " + alt)
            evaled match {
              case Cst(b) => if(b != 0) State(conseq, e, s, k) else State(alt, e, s, k)
              case Code(c) =>
                if(recursionDepth > 10000) {
                  println(s"ERROR: evalms If($c, $conseq, $alt) recursion limit reached...Exiting")
                  System.exit(1)
                }
                val reifConseq = reifyc(inject(conseq, e, s, false))
                val reifAlt = reifyc(inject(alt, e, s, false))
                recursionDepth = 0
                applyCont(k, reflectc(If(c, reifConseq, reifAlt)), null, e)
            }
          case Let(v, exp, body) =>
            State(exp, e, s, LetK(v, body, e, k))

          // Function application
          case App(f, es) =>
            // NB: staging decision done in ``applyProc''
            val Answer(proc, newStore, newEnv) = evalms(State(f, e, s, Halt())) // TODO: should be evalms or evalAtom?
            val args = es.map({ x =>
                                inject(x, newEnv, newStore, false) // TODO: use newEnv or old e for inject
                              })
            // println("DEBUGGING from app(): " + f + "[-->" + proc + "<--]" + " " + args)
            // println("DEBUGGING from app() proc ops: " + proc.asInstanceOf[Clo].state.s(proc.asInstanceOf[Clo].state.e("ops")))
            // println("DEBUGGING from app() ops: " + newStore(newEnv("ops")))
            val newProc = proc match {
              case clo: Clo => Clo(clo.asInstanceOf[Clo].f, State(null, newEnv, newStore, null))
              case code: Code => proc
            }
            val ret = applyProc(newProc, args, newStore, k)
            // println("DEBUGGING from app() ret: " + ret)
            ret

          case Letrec(exps, body) => // Letrec(List((v1, e1), (v2, e2) ..., (vn, en)), body)]
            val (vs, es) = exps.unzip
            // val addrs = vs.map({ x: Var => x.s.hashCode() })
            val addrs = vs.map({ _ => fresh() })
            val varNames = vs.map({x: Var => x.s})
            val updatedEnv = updateMany(e, varNames, addrs)
            val vals = es.map({ x => inject(x, updatedEnv, s, false) })
            val updatedStore = updateMany(s, addrs, vals)
            State(body, updatedEnv, updatedStore, k)

          case VarargLet(exps, body) => {
            val t = inject(exps, e, s, false)

            // Extract list of (var, val) pairs from input "exps" tuple
            val (vs, vals) = ELisp.tupToTupList(t).map({ x => x match {
              case Tup(vr: Str, Tup(vl, ELisp.parser.N)) => (vr.s, inject(ELisp.trans(vl, Nil), e, s, false))
            }}).unzip

            val addrs = vs.map({ _ => fresh() })
            val updatedEnv = updateMany(e, vs, addrs)
            val updatedStore = updateMany(s, addrs, vals)

            val newBody = injectEnvForVarargs(vs.map({x => Var(x)}), body)
            println(vals)
            println(newBody)
            State(newBody, updatedEnv, updatedStore, k)
          }

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
              case _ => Str(s"ERROR: $v does not have the correct list structure for a 'set-car!'")
            }
            val ret = applyCont(k, value, s, e)
            ret

          case Lift(exp) =>
            val trans = State(exp, e, s, Halt())
            val evaled = evalms(trans).asInstanceOf[Answer].v
            val lifted = lift(evaled)
            applyCont(k, Code(lifted), s, e)

          case Run(b,exp) =>
            // first argument decides whether to generate
            // `run` statement or run code directly
            evalms(State(b, e, s, Halt())).asInstanceOf[Answer].v match {
              case Code(b1) =>
                applyCont(k, 
                          reflectc(Run(b1, reifyc(inject(exp, e, s, false)))),
                          s, e)
              case _ =>
                // TODO: verify stFresh reset not needed
                // TODO: do we really need to leak stBlock like this?
                // var oldBlock = LinkedHashMap[String, Exp]()
                // var ans = Answer(null,null,null)
                // val code = reifyc({ /*stFresh = env.length;*/ ans = evalms(State(exp, e, s, Halt())).asInstanceOf[Answer]; oldBlock = stBlock; ans.v })
                // val ret = reifyv(inject(code, ans.e, ans.s, false))
                // applyCont(k,
                //           ret,
                //           s, e)
                val code = reifyc({ /*stFresh = env.length;*/ evalms(State(exp, e, s, Halt())).asInstanceOf[Answer].v })
                applyCont(k,
                          reifyv(inject(code, e, s, false)),
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
    case Lit(_) | Sym(_) | Lam(_, _) | VarargLam(_, _) | Cons(_, _) | Var(_) | Cons_(_, _) | _: Primitive => true
  }

  def evalAtom(c: Exp, e: Env, s: Store): Val = c match {
      case Sym(str) => Str(str)

       // TODO: find a less difficult to debug way to do this.
       //       This just to let unknown values at evaluation time be reflected
       //       to code values instead
      case Var(str) => val ret = s(e(str)); /*println("VAR: " + str + " ===> " + ret);*/if(ret == Str(initStoreErrorStr)) Code(Var(str)) else ret
      case Lit(num) => Cst(num)
      case IsNum(e1) =>
        inject(e1,e,s,false) match {
          case (Code(s1)) =>
            reflectc(IsNum(s1))
          case v => 
            Cst(if (v.isInstanceOf[Cst]) 1 else 0)
        }
      case IsStr(e1) => 
        inject(e1,e,s,false) match {
          case (Code(s1)) =>
            reflectc(IsStr(s1))
          case v => 
            Cst(if (v.isInstanceOf[Str]) 1 else 0)
        }
      case IsCons(e1) =>
        inject(e1,e,s,false) match {
          case (Code(s1)) =>
            reflectc(IsCons(s1))
          case v => 
            Cst(if (v.isInstanceOf[Tup]) 1 else 0)
        }
      case IsCell(e1) =>
        inject(e1,e,s,false) match {
          case (Code(s1)) =>
            reflectc(IsCell(s1))
          case v => 
            Cst(if (v.isInstanceOf[Cell]) 1 else 0)
        }
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
      case lam: Lam => /*println("DEBUGGING from lam ops: " + s(e("ops")) + "[-->" + lam + "<--]");*/ Clo(lam, State(null, e, s, null))

      /* The complexity is the price we pay for multi-argument
      ** lambda support in the EPink evaluator. We use
      ** check the lambda argument list names here
      ** and then buid a regular lambda (i.e. Lam()) but inject a functional
      ** store into the body of the lambda in case the expression evaluated
      ** in the body requests a variable from the argument list.
      */
      case VarargLam(vs, body) => {

        // Get variable name list.
        // If lambda was constructed in Pink using (...)
        // as the argument list we match what the user program
        // that pink is evaluating has declared as the argument
        // list. I.e. we turn (...) into Tup(Var(x), Tup(Var(y), N))
        // if a Pink program declared (lambda (x y) <body>)
        val t = inject(vs, e, s, false)
        val vars = (ELisp.tupToList(t)).map({ x: String => Var(x) }) // TODO: move tupToList and other helpers to separate module

        val newBody = injectEnvForVarargs(vars, body)

        Clo(Lam(vars, newBody), State(null, e, s, null))
      }

      case Plus(e1, e2) =>
        val ret1 = deref(evalms(State(e1, e, s, Halt())).asInstanceOf[Answer].v)
        val ret2 = deref(evalms(State(e2, e, s, Halt())).asInstanceOf[Answer].v)
        (ret1, ret2) match {
          case (Cst(n1), Cst(n2)) => Cst(n1 + n2)
          case (Code(n1),Code(n2)) => reflectc(Plus(n1, n2))
          case (Code(n1),n2) => reflectc(Plus(n1, lift(n2)))
          case _ => Str(s"Cannot perform + operation on expressions $ret1 and $ret2") // ? should be error instead
        }
      case Minus(e1, e2) =>
        val ret1 = deref(evalms(State(e1, e, s, Halt())).asInstanceOf[Answer].v)
        val ret2 = deref(evalms(State(e2, e, s, Halt())).asInstanceOf[Answer].v)
        (ret1, ret2) match {
          case (Cst(n1), Cst(n2)) => Cst(n1 - n2)
          case (Code(n1),Code(n2)) => reflectc(Minus(n1, n2))
          case (Code(n1),n2) => reflectc(Minus(n1, lift(n2)))
          case _ => Str(s"Cannot perform - operation on expressions $e1 and $e2") // ? should be error instead
        }
      case Times(e1, e2) =>
        val ret1 = deref(evalms(State(e1, e, s, Halt())).asInstanceOf[Answer].v)
        val ret2 = deref(evalms(State(e2, e, s, Halt())).asInstanceOf[Answer].v)
        (ret1, ret2) match {
          case (Cst(n1), Cst(n2)) => Cst(n1 * n2)
          case (Code(n1),Code(n2)) => reflectc(Times(n1, n2))
          case (Code(n1),n2) => reflectc(Times(n1, lift(n2)))
          case _ => Str(s"Cannot perform * operation on expressions $e1 and $e2 (evaluated to $ret1 $ret2 respectively)") // ? should be error instead
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
        // println("FST: s ==> " + deref(s(e("s"))))
        inject(e1, e, s, false) match {
          case Cell(k, idx) => cells(k)(idx) match {
                                  case Cell(k2, idx2) => Cell(k2, 0)
                                  case other => other // Cell(k, 0)
                                }
          case Code(a) => reflectc(Fst_(a))
          case Tup(a, b) => a
          case Str(s) if(s == initStoreErrorStr) => reflectc(e1) // Error because variable not in scope *yet*
          case Cst(_) | Str(_) => Str(s"ERROR: $e1 does not have correct list structure to perform a 'car'")
        }
      case Snd_(e1) =>
        inject(e1, e, s, false) match {
          case Cell(k, idx) => cells(k)(scala.math.min(1, idx + 1)) match {
                                  case Cell(k2, idx2) => Cell(k2, 0)
                                  case otherwise => otherwise // Cell(k, scala.math.min(1, idx + 1))
                                }
          case Code(a) => reflectc(Snd_(a))
          case Tup(a, b) => b // TODO: Tup should be handled in lisp-frontend and not here. This is
                              //       curently for '(...) to work
          case Str(s) if(s == initStoreErrorStr) => reflectc(e1) // Error because variable not in scope *yet*
          case Cst(_) | Str(_) => Str(s"ERROR: $e1 does not have correct list structure to perform a 'cdr'")
        }
        
      case Equ(e1, e2) =>
        val ret1 = evalms(State(e1, e, s, Halt())).asInstanceOf[Answer].v
        val ret2 = evalms(State(e2, e, s, Halt())).asInstanceOf[Answer].v
        // println("EQU: " + ret1 + "[-->" + e1 + "<--] #### " + ret2 + "[-->" + e2 + "<--]")
        (ret1, ret2) match {
          case (v1, v2) if !v1.isInstanceOf[Code] && !v2.isInstanceOf[Code] => Cst(if (v1 == v2) 1 else 0)

          case (Code(n1: Lit),Code(n2: Lit)) => Cst(if (n1 == n2) 1 else 0)
          case (Code(n1),Code(n2)) => reflectc(Equ(n1, n2))

          case (Code(Lit(n1)), Cst(n2)) => Cst(if (n1 == n2) 1 else 0)
          case (Cst(n1), Code(Lit(n2))) => Cst(if (n1 == n2) 1 else 0)
          
          case (Code(v1), s2) => reflectc(Equ(v1, lift(s2)))
          case (s1, Code(v2)) => reflectc(Equ(lift(s1), v2))
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

      case And(e1,e2) =>
        val ret1 = inject(e1, e, s, false)
        val ret2 = inject(e2, e, s, false)
        (ret1, ret2) match {
          case (Cst(n1), Cst(n2)) =>
            Cst(if (n1 == 1 && n2 == 1) 1 else 0)
          case (Code(s1),Code(s2)) =>
            reflectc(And(s1,s2))
          case _ => Str(s"Cannot perform && operation on expressions $ret1 and $ret2") // ? should be error instead
        }
    }

  def injectEnvForVarargs(vars: List[Var], body: Exp): Exp = {
    // Create the functional store:
    // It is a lambda that takes a single argument
    // and checks whether it matches any of the variables
    // in the argument list. If it finds a match, return
    // the variable matching the argument, otherwise
    // defer to an environment lookup
    def aux(exp: List[Var]): Exp = exp match {
      case hd::tl => If(Equ(Var("__z__"), Sym(hd.s)), hd, aux(tl))
      case _ => App(Var("env"),List(Var("__z__")))
    }

    val injectionBranches = aux(vars)
    val envInjection = Lam(List(Var("__z__")), injectionBranches)

    // We replace the (...) placeholder with the funcional store
    // from above
    val bodyApp = body.asInstanceOf[App]
    val i = (bodyApp.arg).lastIndexOf(Sym("..."))
    val newArgs = (bodyApp.arg).updated(i, envInjection)
    val newBody = App(bodyApp.f, newArgs)
    newBody
  }

  def update[A,B](store: StoreFun[A, B], a: A, b: B): StoreFun[A, B] = {
    x: A =>
      if(x == a)
        b
      else {
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
    case Clo(Lam(vs, body), State(_, env, store, _)) =>
      val addrs = vs.map({_ => fresh()})
      val varNames = vs.map({x: Var => x.s})
      val updatedEnv = updateMany(env, varNames, addrs)
      val updatedStore = updateMany(s, addrs, args)

      val sizeDiff = vs.size - args.size
      if(sizeDiff > 0) {
        // Curry function
        State(Lam(vs.drop(args.size), body), updatedEnv, updatedStore, k)
      } else if(args.size == vs.size) {
        // Evaluate body
        State(body, updatedEnv, updatedStore, k)
      } else {
        State(Sym("ERROR: tried to apply closure to too many arguments"), null, null, Halt())
      }

    // TODO: check if all args are Code as well?
    case Code(s1) =>
      val codeArgs = args.map( {a => a match {
        case Code(res: Exp) => res
        case otherwise: Val => lift(otherwise)
      }})
      applyCont(k, reflectc(App(s1, codeArgs)), s, null)
  }

  // TODO: should be more robust
  val initStoreErrorStr = "Error: using init store"
  val initEnv = {arg: String => -1}
  val initStore = {arg: Int => Str(initStoreErrorStr)}

  def inject(e: Exp, env: Env = initEnv, store: Store = initStore, reset: Boolean = true) = {
    if(reset) {
      recursionDepth = 0
      stFresh = 0
      stBlock = LinkedHashMap.empty
      cells = LinkedHashMap[String, List[Val]]() // TODO: could be made regular HashMap
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