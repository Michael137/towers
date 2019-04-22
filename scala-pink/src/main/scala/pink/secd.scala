import Pink._
import Lisp._
import Base._

object SECD_Machine {
  val src = """
(let deeplift (lambda l x (if (code? x) x (if (pair? x) (lift (cons (l (car x)) (l (cdr x)))) (lift x))))
(let deeplift-if-code (lambda _ r (lambda _ x (if (code? r) (deeplift x) x)))
(let locate (lambda locate i (lambda _ j (lambda _ env
(let loc (lambda loc y (lambda _ lst
(if (eq? y 1) (car lst) ((loc (- y 1)) (cdr lst)))))
((loc j) ((loc i) env))
))))
(let machine (lambda machine s (lambda _ e (lambda _ c (lambda _ d
(let _ (log 0 c)
(if (eq? 'NIL (car c)) ((((machine (cons '() s)) e) (cdr c)) d)
(if (eq? 'LDC (car c)) ((((machine (cons (cadr c) s)) e) (cddr c)) d)
(if (eq? 'LD (car c))
  (let ij (cadr c) (let i (car ij) (let j (cadr ij)
  ((((machine (cons (((locate i) j) e) s)) e) (cddr c)) d))))
(if (eq? 'LIFT (car c)) ((((machine (cons (log 0 (lift (log 0 (car s)))) (cdr s))) e) (cdr c)) d)
(if (eq? 'CAR (car c)) ((((machine (cons (car (car s)) (cdr s))) e) (cdr c)) d)
(if (eq? 'CDR (car c)) ((((machine (cons (cdr (car s)) (cdr s))) e) (cdr c)) d)
(if (eq? 'ADD (car c)) ((((machine (cons (+ (car s) (cadr s)) (cddr s))) e) (cdr c)) d)
(if (eq? 'SUB (car c)) ((((machine (cons (- (car s) (cadr s)) (cddr s))) e) (cdr c)) d)
(if (eq? 'MPY (car c)) ((((machine (cons (* (car s) (cadr s)) (cddr s))) e) (cdr c)) d)
(if (eq? 'EQ (car c)) ((((machine (cons (eq? (car s) (cadr s)) (cddr s))) e) (cdr c)) d)
(if (eq? 'CONS (car c)) ((((machine (cons (cons (car s) (cadr s)) (cddr s))) e) (cdr c)) d)
(if (eq? 'SEL (car c))
  (if (car s)
    ((((machine (cdr s)) e) (cadr c))  (cons (cdddr c) d))
    ((((machine (cdr s)) e) (caddr c)) (cons (cdddr c) d)))
(if (eq? 'JOIN (car c)) ((((machine s) e) (car d)) (cdr d))
(if (eq? 'LDF (car c))
  (let f (cadr c)
  (let fun (lambda fun xs ((((machine '()) (cons (cons fun xs) e)) f) '()))
  ((((machine (cons fun s)) e) (cddr c)) d)))
(if (eq? 'AP (car c))
  (let fun (car s)
  (let vs ((deeplift-if-code fun) (cadr s))
  (let r (fun vs)
  ((((machine (cons r (cddr s))) e) (cdr c)) d))))
(if (eq? 'RTN (car c))
  (car s)
(if (eq? 'STOP (car c)) s
(if (eq? 'WRITEC (car c)) (car s)
(cons 'ERROR c))))))))))))))))))))))))
(lambda _ c ((((machine '()) '()) c) '()))))))
"""

  val evl = src
  def run(src: String) = {
    ev(s"($evl '($src))")
  }

  def test() = {
    println("// ------- SECD_Machine.test --------")

    check(ev(s"($evl '(LDC 1 WRITEC))"))("Cst(1)")
    check(ev(s"($evl '(LDC 1 LDC 2 ADD WRITEC))"))("Cst(3)")
    check(ev(s"($evl '(LDC 2 SEL (LDC 1 JOIN) (LDC 0 JOIN) WRITEC))"))("Cst(1)")
    check(ev(s"($evl '(NIL LDC 2 CONS LDC 1 CONS LDF (LDC 2 LDC 1 ADD RTN) AP WRITEC))"))("Cst(3)")
    check(ev(s"($evl '(NIL LDC 2 CONS LDC 1 CONS LDF (LD (1 3) LD (1 2) ADD RTN) AP WRITEC))"))("Cst(3)")
    check(ev(s"($evl '(NIL LDC 6 CONS LDF (LD (1 2) SEL (NIL LDC 1 LD (1 2) SUB CONS LD (1 1) AP LD (1 2) MPY JOIN) (LDC 1 JOIN) RTN) AP WRITEC))"))("Cst(720)")
  }
}

object SECD_Compiler {
  // Takes input S-expressions
  // Outputs SECD instructions
  import Lisp.parser._

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

  def compile(e: Val, env: CompEnv, acc: Val): Val = e match {
    // Null, Number or Identifier
    case N => Tup(Str("NIL"), acc)
    case v: Str => {
      val ij = index(v, env)
      Tup(Str("LD"), Tup(ij, acc))
    }
    case n: Cst => Tup(Str("LDC"), Tup(n, acc))
    // Builtin, Lambda, Special form
    case Tup(Str("lambda"), Tup(args, Tup(body,N))) =>
      compileLambda(body, (Str("_")::tupToList(args))::env, acc)
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
    case Tup(Str("if"), Tup(c,Tup(a,Tup(b,N)))) =>
      compileIf(c, a, b, env, acc)
    case Tup(Str("let"), Tup(vs, Tup(vals, Tup(body, N)))) => {
      val newEnv = (Str("_")::tupToList(vs))::env
      Tup(Str("NIL"), compileApp(vals, env, compileLambda(body, newEnv, Tup(Str("AP"), acc))))
    }
    case Tup(Str("letrec"), Tup(Tup(name, N), Tup(Tup(Tup(Str("lambda"), Tup(args, Tup(body,N))), N), Tup(letrec_body, N)))) => {
      val newEnv = (Str("_")::name::Nil)::env
      Tup(Str("NIL"), compileLambda(body, (name::tupToList(args))::env, Tup(Str("CONS"), compileLambda(letrec_body, newEnv, Tup(Str("AP"), acc)))))
    }
    // Application
    case Tup(fn, args) => Tup(Str("NIL"), compileApp(args, env, compile(fn, env, Tup(Str("AP"), acc))))
  }

  def compileExp(src: String) = {
    val instrs = compile(parseExp(src), Nil, Tup(Str("WRITEC"), N))
    val instrSrc = instrsToString(instrs)
    println(instrSrc)
    instrSrc
  }

  def compileAndRun(src: String) = {
    SECD_Machine.run(compileExp(src))
  }

  def prettycode(v: Val) = {
    Base.pretty(Base.reifyc(v), Nil)
  }

  def test() = {
    println("// ------- SECD_Compiler.test --------")

    check(compileAndRun("(letrec (fac) ((lambda (n) (if (eq? n 0) 1 (* n (fac (- n 1)))))) (fac 6))"))("Cst(720)")
    check(compileAndRun("(letrec (fac) ((lambda (n) (if (eq? n 0) 1 (* (fac (- n 1)) n)))) (fac 6))"))("Cst(720)")
    check(compileAndRun(VMMatcher.matcher("'(_ * a _ * done)", "'(b a done)")))("Str(yes)")

    println(prettycode(compileAndRun("(+ (lift 1) (lift 2))")))
    println(prettycode(compileAndRun("(lift (lambda (x) (lift 1)))")))
    println(prettycode(compileAndRun("(lift (lambda (x) x))")))
    println(prettycode(compileAndRun("(lift (lambda (x) (+ x (lift 1))))")))

    println(prettycode(compileAndRun(VMLiftedMatcher.lifted_matcher("'(a done)"))))

    println(prettycode(compileAndRun(VMLiftedMatcher.lifted_matcher("'(a * done)"))))

    testDone()
  }
}
