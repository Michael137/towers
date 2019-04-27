import Pink._
import Lisp._
import Base._

object SECD_Machine {
  val src = """
(let deeplift (lambda l x (if (code? x) x (if (pair? x) (lift (cons (l (car x)) (l (cdr x)))) (lift x))))
(let deeplift-if-code (lambda _ r (lambda _ x (if (code? r) (deeplift x) x)))
(let null? (lambda _ x (eq? ((deeplift-if-code x) '()) x))
(let atom? (lambda _ a (or (sym? a) (num? a)))
(let assq (lambda assq x (lambda _ l (if (null? l) '() (if (eq? (caar l) x) (car l) ((assq x) (cdr l))))))
(let locate (lambda locate i (lambda _ j (lambda _ env
(let loc (lambda loc y (lambda _ lst
(if (eq? y 1) (car lst) ((loc (- y 1)) (cdr lst)))))
((loc j) ((loc i) env))
))))
(let funs (cons '() '())
(let machine (lambda machine s (lambda _ e (lambda _ c (lambda _ d
(let lft (lambda _ x (if (and (pair? x) (eq? '__clo (car x)))
   (let memo (cons (lift '()) '())
   (let _ (set-car! funs (cons (cons (cdr x) memo) (car funs)))
   (lift (lambda fun args
     (let _ (set-car! memo fun)
     ((((machine '()) (cons args (cdr (cdr x)))) (car (cdr x))) 'ret))))))
   (lift x)))
(let _ _
(if (eq? 'NIL (car c)) ((((machine (cons '() s)) e) (cdr c)) d)
(if (eq? 'LDC (car c)) ((((machine (cons (cadr c) s)) e) (cddr c)) d)
(if (eq? 'LD (car c))
  (let ij (cadr c) (let i (car ij) (let j (cadr ij)
  ((((machine (cons (((locate i) j) e) s)) e) (cddr c)) d))))
(if (eq? 'LIFT (car c)) ((((machine (cons (lft (car s)) (cdr s))) e) (cdr c)) d)
(if (eq? 'CAR (car c)) ((((machine (cons (car (car s)) (cdr s))) e) (cdr c)) d)
(if (eq? 'CDR (car c)) ((((machine (cons (cdr (car s)) (cdr s))) e) (cdr c)) d)
(if (eq? 'CADR (car c)) ((((machine (cons (cadr (car s)) (cdr s))) e) (cdr c)) d)
(if (eq? 'CADDR (car c)) ((((machine (cons (caddr (car s)) (cdr s))) e) (cdr c)) d)
(if (eq? 'CADDDR (car c)) ((((machine (cons (cadddr (car s)) (cdr s))) e) (cdr c)) d)
(if (eq? 'CDDDR (car c)) ((((machine (cons (cdddr (car s)) (cdr s))) e) (cdr c)) d)
(if (eq? 'EMPTY? (car c)) ((((machine (cons (null? (car s)) (cdr s))) e) (cdr c)) d)
(if (eq? 'ATOM? (car c)) ((((machine (cons (atom? (car s)) (cdr s))) e) (cdr c)) d)
(if (eq? 'SYM? (car c)) ((((machine (cons (sym? (car s)) (cdr s))) e) (cdr c)) d)
(if (eq? 'NUM? (car c)) ((((machine (cons (num? (car s)) (cdr s))) e) (cdr c)) d)
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
  ((((machine (cons (cons '__clo (cons (cadr c) e)) s)) e) (cddr c)) d)
(if (eq? 'AP (car c))
  (let r (if (code? (car s)) (cons '() (cons (car s) '())) ((assq (cdr (car s))) (car funs)))
  (if (eq? r '())
  (let f (car (cdr (car s)))
  (let ep (cdr (cdr (car s)))
  (let v (cadr s)
  ((((machine '()) (cons v ep)) f) (cons (cddr s) (cons e (cons (cdr c) d)))))))
  (let fun (cadr r)
  (let v (cadr s)
  ((((machine (cons (fun ((deeplift-if-code fun) v)) (cddr s))) e) (cdr c)) d)))))
(if (eq? 'RTN (car c))
  (if (eq? d 'ret) (car s)
  ((((machine (cons (car s) (car d))) (cadr d)) (caddr d)) (cdddr d)))
(if (eq? 'DUM (car c))
  ((((machine s) (cons '() e)) (cdr c)) d)
(if (eq? 'RAP (car c))
  (let f (car (cdr (car s)))
  (let ep (cdr (cdr (car s)))
  (let v (cadr s)
  ((((machine '()) (set-car! ep v)) f) (cons (cddr s) (cons (cdr e) (cons (cdr c) d)))))))
(if (eq? 'STOP (car c)) s
(if (eq? 'WRITEC (car c)) (car s)
(cons 'ERROR c)))))))))))))))))))))))))))))))))))
(lambda _ c ((((machine '()) '()) c) '()))))))))))
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
    check(ev(s"($evl '(NIL LDC 2 CONS LDC 1 CONS LDF (LD (1 2) LD (1 1) ADD RTN) AP WRITEC))"))("Cst(3)")
    ev(s"($evl '(DUM STOP))")
    val factorialProg = """'(NIL LDC 1 CONS LDC 6 CONS LDF
                (DUM NIL LDF
                (LDC 0 LD (1 1) EQ SEL
                (LD (1 2) JOIN)
                (NIL LD (1 2) LD (1 1) MPY CONS
                LD (3 2) LD (1 1) SUB CONS LD (2 1) AP JOIN)
                RTN)
                CONS LDF
                (NIL LD (2 2) CONS LD (2 1) CONS LD (1 1) AP RTN) RAP
                RTN) AP WRITEC)"""
    check(ev(s"($evl $factorialProg)"))("Cst(720)")
    check(ev(s"""($evl '(
DUM NIL LDF
(LDC 0 LD (1 1) EQ SEL
(LDC 1 JOIN)
(LD (1 1) NIL LDC 1 LD (1 1) SUB CONS LD (2 1) AP MPY JOIN)
RTN)
CONS LDF
(NIL LDC 6 CONS LD (1 1) AP RTN) RAP WRITEC
))"""))("Cst(720)")
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
      compileLambda(body, tupToList(args)::env, acc)
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
      val newEnv = tupToList(vs)::env
      Tup(Str("NIL"), compileApp(vals, env, compileLambda(body, newEnv, Tup(Str("AP"), acc))))
    }
    case Tup(Str("letrec"), Tup(vs, Tup(vals, Tup(body, N)))) => {
      val newEnv = tupToList(vs)::env
      Tup(Str("DUM"), Tup(Str("NIL"),
        compileApp(vals, newEnv, compileLambda(body, newEnv, Tup(Str("RAP"), acc)))))
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

  def compileAndRunLifted(src: String, arg: String) = {
    val v = compileAndRun(src)
    println(prettycode(v))
    val e = Base.reifyc(v)
    reifyv(evalms(Nil, App(e,
      Cons(trans(parseExp(arg), Nil), Sym("."))
    )))
  }

  def lifted_meta_eval(p: String) = meta_eval(p, "lift")

  def meta_eval(p: String, lift: String = "(lambda (x) x)") = s"""
(letrec (eval) ((lambda (exp env)
(if (sym? exp)
  (env exp)
(if (num? exp)
  ($lift exp)
(if (eq? (car exp) 'lift)
  (lift (eval (cadr exp) env))
(if (eq? (car exp) 'car)
  (car (eval (cadr exp) env))
(if (eq? (car exp) 'cdr)
  (cdr (eval (cadr exp) env))
(if (eq? (car exp) '+)
  (+ (eval (cadr exp) env) (eval (caddr exp) env))
(if (eq? (car exp) '-)
  (- (eval (cadr exp) env) (eval (caddr exp) env))
(if (eq? (car exp) '*)
  (* (eval (cadr exp) env) (eval (caddr exp) env))
(if (eq? (car exp) 'eq?)
  (eq? (eval (cadr exp) env) (eval (caddr exp) env))
(if (eq? (car exp) '>)
  (> (eval (cadr exp) env) (eval (caddr exp) env))
(if (eq? (car exp) 'quote)
  ($lift (cadr exp))
(if (eq? (car exp) 'if)
  (if (eval (cadr exp) env) (eval (caddr exp) env) (eval (cadddr exp) env))
(if (eq? (car exp) 'let)
  (let (x)
    ((eval (car (caddr exp)) env))
    (eval (cadddr exp) (lambda (z) (if (eq? z (car (cadr exp))) x (env z)))))
(if (eq? (car exp) 'letrec)
  (let (r) ((car (caddr exp)))
  (letrec (f)
    ((lambda (x) (eval (caddr r) (lambda (z) (if (eq? z (car (cadr exp))) f (if (eq? z (car (cadr r))) x (env z)))))))
    (eval (cadddr exp) (lambda (z) (if (eq? z (car (cadr exp))) ($lift f) (env z))))))
(if (eq? (car exp) 'lambda)
  ($lift (lambda (x) (eval (caddr exp) (lambda (y) (if (eq? y (car (cadr exp))) x (env y))))))
((eval (car exp) env) (eval (cadr exp) env)))))))))))))))))))
(eval (quote $p) '()))
"""

  def test() = {
    println("// ------- SECD_Compiler.test --------")

    check(compileAndRun("(letrec (fac) ((lambda (n) (if (eq? n 0) 1 (* n (fac (- n 1)))))) (fac 6))"))("Cst(720)")
    check(compileAndRun("(letrec (fac) ((lambda (n) (if (eq? n 0) 1 (* (fac (- n 1)) n)))) (fac 6))"))("Cst(720)")

    println(prettycode(compileAndRun("(lift (letrec (fac) ((lambda (n) (if (eq? n (lift 0)) (lift 1) (* n (fac (- n (lift 1))))))) fac))")))
    println(prettycode(compileAndRun("(letrec (fac) ((lambda (n) (if (eq? n (lift 0)) (lift 1) (* n (fac (- n (lift 1))))))) (lift fac))")))

    println(prettycode(compileAndRun("(+ (lift 1) (lift 2))")))
    println(prettycode(compileAndRun("(lift (lambda (x) (lift 1)))")))
    println(prettycode(compileAndRun("(lift (lambda (x) x))")))
    println(prettycode(compileAndRun("(lift (lambda (x) (+ x (lift 1))))")))

    check(compileAndRun(meta_eval("(- 1 1)")))("Cst(0)")
    check(compileAndRun(meta_eval("(((lambda (a) (lambda (b) b)) 1) 2)")))("Cst(2)")
    check(compileAndRun(meta_eval("(letrec (fac) ((lambda (n) (if (eq? n 0) 1 (* n (fac (- n 1)))))) (fac 3))")))("Cst(6)")
    check(compileAndRun("(let (x) (1) x)"))("Cst(1)")
    check(compileAndRun(meta_eval("(let (x) (1) x)")))("Cst(1)")
    check(compileAndRun(meta_eval("(let (id) ((lambda (y) y)) (id 1))")))("Cst(1)")
    check(compileAndRun("(let (m) ((lambda (r) (if (eq? 'done (car r)) (lambda (s) 'yes) (lambda (s) 'no)))) ((m '(done)) '(a done)))"))("Str(yes)")
    check(compileAndRun(meta_eval("(let (m) ((lambda (r) (lambda (s) 2))) ((m 3) 4))")))("Cst(2)")
    check(compileAndRun(meta_eval("'yes")))("Str(yes)")
    check(compileAndRun(meta_eval("(letrec (m) ((lambda (r) (if (eq? 'done (car r)) (lambda (s) 'yes) (lambda (s) 'no)))) ((m '(done)) '(a done)))")))("Str(yes)")
    check(compileAndRun(meta_eval("(let (m) ((lambda (r) (if (eq? 'done (car r)) (lambda (s) 'yes) (lambda (s) 'no)))) ((m '(done)) '(a done)))")))("Str(yes)")

    // -1. run matcher on plain meta-eval
    check(compileAndRun(VMMatcher.matcher("'(_ * a _ * done)", "'(b a done)")))("Str(yes)")

    // 0. run matcher on plain meta-eval
    check(compileAndRun(meta_eval(VMMatcher.matcher("'(done)", "'(a done)"))))("Str(yes)")
    check(compileAndRun(meta_eval(VMMatcher.matcher("'(a done)", "'(a done)"))))("Str(yes)")
    check(compileAndRun(meta_eval(VMMatcher.matcher("'(_ * a _ * done)", "'(b a done)"))))("Str(yes)")

    // 1. staged matcher on VM
    check(compileAndRunLifted(VMLiftedMatcher.lifted_matcher("'(a done)"), "'(a done)"))("Str(yes)")
    check(compileAndRunLifted(VMLiftedMatcher.lifted_matcher("'(a * done)"), "'(a done)"))("Str(yes)")

    // 2. Staged matcher on meta-eval
    check(compileAndRunLifted(meta_eval(VMLiftedMatcher.lifted_matcher("'(a done)")), "'(a done)"))("Str(yes)")
    check(compileAndRunLifted(meta_eval(VMLiftedMatcher.lifted_matcher("'(a * done)")), "'(a done)"))("Str(yes)")

    def curried_matcher(p: String) = {
      val r = VMMatcher.matcher(p, "my_input")
      s"(lambda (my_input) $r)"
    }
    // 3. matcher on staged meta-eval
    check(compileAndRunLifted(lifted_meta_eval(curried_matcher("'(a done)")), "'(a done)"))("Str(yes)")
    check(compileAndRunLifted(lifted_meta_eval(curried_matcher("'(a * done)")), "'(a done)"))("Str(yes)")

    testDone()
  }
}
