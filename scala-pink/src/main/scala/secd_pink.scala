// Thanks to Dr. Nada Amin

import Base._
import Lisp._

object SECD {
  val src = """
(let debug_mode? 0
(let debug (lambda _ x (if debug_mode? (log 0 x) _))
(let caaar (lambda _ x (car (caar x)))
(let caaaar (lambda _ x (car (caaar x)))
(let cadar (lambda _ x (car (cdr (car x))))
(let cdar (lambda _ x (cdr (car x)))
(let cddr (lambda _ x (cdr (cdr x)))
(let cdddr (lambda _ x (cdr (cddr x)))
(let caar (lambda _ x (car (car x)))
(let liftIfCode (lambda _ a (lambda _ b (if (code? b) (maybe-lift a) a)))
(let null? (lambda _ x (eq? ((liftIfCode '()) x) x))
(let map (lambda map f (lambda mapf xs (if (null? xs) xs (cons (f (car xs)) (mapf (cdr xs))))))
(let mla (lambda mla xs (if (code? xs) xs (if (pair? xs) (maybe-lift (cons (mla (car xs)) (mla (cdr xs)))) (maybe-lift xs))))
(let pla (lambda pla xs (if (code? xs) xs (if (pair? xs) (possible-lift (cons (pla (car xs)) (mla (cdr xs)))) (possible-lift xs))))
(let atom? (lambda atom? a (if (sym? a) (maybe-lift 1) (if (num? a) (maybe-lift 1) (maybe-lift 0))))
(let locate (lambda locate i (lambda _ j (lambda _ env
(let loc (lambda loc y (lambda _ lst
(if (eq? y 1) (car lst) ((loc (- y 1)) (cdr lst)))))
((loc j) ((loc i) env))
))))
(let machine (lambda machine s (lambda _ d (lambda _ fns (lambda _ bt (lambda _ ops (lambda _ env
(let _ (debug (cons 'ops ops))
(let _ (debug (cons 'stack s))
(let _ (debug (cons 'env env))
(let _ (debug (cons 'dump d))
(let _ (debug (cons 'fns fns))
(let _ (debug '##################>>>)
(if (eq? 'LIFT (car ops))
 (let r (if (num? (car s))
      (lift (car s))
      (lift (lambda _ x ((((((machine '()) 'ret) fns) bt) (car (cdr (car s)))) (cons (cons x '()) (cdr (cdar s)))))))
 ((((((machine (cons r (cdr s))) d) fns) bt) (cdr ops)) env))
(if (eq? 'STOP (car ops)) (pla (mla s))
(if (eq? 'WRITEC (car ops)) (pla (car s))
(if (eq? 'LDC (car ops))
((((((machine (cons (maybe-lift (cadr ops)) s)) d) fns) bt) (cddr ops)) env)
(if (eq? 'ADD (car ops))
((((((machine (cons (+ (car s) (cadr s)) (cddr s))) d) fns) bt) (cdr ops)) env)
(if (eq? 'SUB (car ops))
((((((machine (cons (- (car s) (cadr s)) (cddr s))) d) fns) bt) (cdr ops)) env)
(if (eq? 'LD (car ops))
((((((machine (cons (((locate (car (cadr ops))) (cadr (cadr ops))) env) s)) d) fns) bt) (cddr ops)) env)
(if (eq? 'LDR (car ops))
  ((((((machine (cons (lambda _ _ (((locate (car (cadr ops))) (cadr (cadr ops))) fns)) s)) (cons 'fromldr d)) fns) bt) (cddr ops)) env)
(if (eq? 'LDF (car ops))
  ((((((machine (cons
                  (cons (lambda _ env ((((((machine '()) 'ret) fns) bt) (cadr ops)) env))
                        (cons (cadr ops) env))
                  s)) d) fns) bt) (cddr ops)) env)
(if (eq? 'NIL (car ops))
((((((machine (cons (maybe-lift '()) s)) d) fns) bt) (cdr ops)) env)
(if (eq? 'AP (car ops))
  (if (code? (car s))
    (if lifting?
      (let _ _ ;(log 0 'lifting?====>)
      (let s0 ((car s) (car (cadr s)))
        ((((((machine (cons s0 (cddr s))) d) fns) bt) (cdr ops)) env)))
      (let fun (caar s)
        (let _ _ ;(log 0 'code?====>)
        (let s (cons (cdar s) (cdr s))
          (let s0 (fun (mla (cons (cadr s) (cdar s))))
            ((((((machine (cons s0 (cddr s))) d) fns) bt) (cdr ops)) env))))))
  (if (not (pair? (car s)))
    (let _ _ ;(debug '!pair?====>)
    (let d (if (eq? 'fromldr (car d))
          (cdr d)
          d)
      (let s1 ((car s) 1)
        (let s0 ((car s1) (mla (cons (cadr s) (cdr s1))))
          ((((((machine (cons s0 (cddr s))) d) fns) bt) (cdr ops)) env)))))
  (let _ _ ;(debug 'else?====>)
  (if (lambda? (caar s))
    (let _ _ ;(debug 'lambda?====>)
    (let fun (caar s)
    (let s (cons (cdar s) (cdr s))
        ;((((((machine '()) (cons (cddr s) (cons env (cons (cdr ops) d)))) fns) bt) (caar s)) (cons (cadr s) (cdar s)))
        (let s0 (fun (mla (cons (cadr s) (cdar s))))
            ((((((machine (cons s0 (cddr s))) d) fns) bt) (cdr ops)) env)))))
  (if (eq? 'try (caaar s))
    'ERROR
  'ERROR)))))

(if (eq? 'RTN (car ops))
  (if (and (pair? d)
           (eq? 'fromldr (car d)))
    (if (code? (car env)) ;this means "RTN" was called immediately after "LDR"
      (mla (let s1 ((car s) 1)
        (cons
          (lambda _ arg ((car s1) (mla (cons (car arg) (cdr s1)))))
          (cdr s1))))
      (car s))
  (if (eq? 'ret d)
    (mla (car s))
  (if (eq? 'rec (car d))
    (let _ (debug s)
      (mla (car s)))
    ((((((machine (cons (car s) (car d))) (cddddr d)) (cadddr d)) bt) (caddr d)) (cadr d)))))
(if (eq? 'CONS (car ops))
((((((machine (cons (cons (car s) (cadr s)) (cddr s))) d) fns) bt) (cdr ops)) env)
(if (eq? 'SEL (car ops))
  (if (car s)
    ((((((machine (cdr s)) (cons (cdddr ops) d)) fns) bt) (cadr ops)) env)
    ((((((machine (cdr s)) (cons (cdddr ops) d)) fns) bt) (caddr ops)) env))
(if (eq? 'JOIN (car ops))
  ((((((machine s) (cdr d)) fns) bt) (car d)) env)
(if (eq? 'MPY (car ops))
  ((((((machine (cons (* (car s) (cadr s)) (cddr s))) d) fns) bt) (cdr ops)) env)
(if (eq? 'EQ (car ops))
((((((machine (cons (eq? (car s) (cadr s)) (cddr s))) d) fns) bt) (cdr ops)) env)
(if (eq? 'GT (car ops))
(let _ _
((((((machine (cons (> (car s) (cadr s)) (cddr s))) d) fns) bt) (cdr ops)) env))
(if (eq? 'LT (car ops))
((((((machine (cons (< (car s) (cadr s)) (cddr s))) d) fns) bt) (cdr ops)) env)
(if (eq? 'DUM (car ops))
((((((machine s) d) fns) bt) (cdr ops)) (cons (maybe-lift '()) env))
(if (eq? 'RAP (car ops))
  (let s1 (cadr s)
    (let s1 (cons (cdar s1) (cdr s1))
      (let newDump (cons (cddr s) (cons env (cons (cdr ops) (cons fns d))))
        (let _ (debug (cadar s))
          (let rec (maybe-lift (lambda rec env ((((((machine '()) (cons 'rec newDump)) (cons (cons (cons rec (cdar s1)) (cdr s1)) fns)) bt) (caar s1)) env)))
            ((((((machine '()) newDump) (cons (cons (cons rec (cdar s1)) (cdr s1)) fns)) bt) (cadar s)) env))))))
(if (eq? 'CAR (car ops))
((((((machine (cons (car (car s)) (cdr s))) d) fns) bt) (cdr ops)) env)
(if (eq? 'CDR (car ops))
((((((machine (cons (cdar s) (cdr s))) d) fns) bt) (cdr ops)) env)
(if (eq? 'QUOTE (car ops))
((((((machine (cons (cons (car s) '()) (cdr s))) d) fns) bt) (cdr ops)) env)
(if (eq? 'CADR (car ops))
((((((machine (cons (cadr (car s)) (cdr s))) d) fns) bt) (cdr ops)) env)
(if (eq? 'CADDR (car ops))
((((((machine (cons (caddr (car s)) (cdr s))) d) fns) bt) (cdr ops)) env)
(if (eq? 'CADDDR (car ops))
((((((machine (cons (cadddr (car s)) (cdr s))) d) fns) bt) (cdr ops)) env)
(if (eq? 'CDDDR (car ops))
((((((machine (cons (cdddr (car s)) (cdr s))) d) fns) bt) (cdr ops)) env)
(if (eq? 'EMPTY? (car ops))
((((((machine (cons (null? (car s)) (cdr s))) d) fns) bt) (cdr ops)) env)
(if (eq? 'ATOM? (car ops))
  ((((((machine (cons (atom? (car s)) (cdr s))) d) fns) bt) (cdr ops)) env)
(if (eq? 'SYM? (car ops))
  ((((((machine (cons (sym? (car s)) (cdr s))) d) fns) bt) (cdr ops)) env)
(if (eq? 'NUM? (car ops))
  ((((((machine (cons (num? (car s)) (cdr s))) d) fns) bt) (cdr ops)) env)
(if (eq? 'DBG (car ops))
  (let _ _ (log 0 (cons 'breakpoint: s)))

(if (eq? 'AP0 (car ops))
  (let s (cons (cdar s) (cdr s))
  (let closure (car s)
    (let newDump (cons (cdr s) (cons env (cons (cdr ops) (cons fns d))))
    (let newOps (car closure)
    (let newEnv (cdr closure)
    ((((((machine '()) newDump) fns) bt) newOps) newEnv))))))
(if (eq? 'CC (car ops))
  (let state (cons (cdr s) (cons env (cons (cdr ops) d)))
  (let closure (car s)
    ((((((machine '()) state) fns) bt) (car closure)) (cons (cons 'cc state) (cdr closure)))))
(if (eq? 'TRY (car ops))
  (let _ _
  (let instrsToTry (cadr ops)
  (let cc (cons 'cc (cons s (cons env (cons (cddr ops) d))))
    ((((((machine s) d) fns) (cons cc bt)) instrsToTry) env))))
(if (eq? 'FAIL (car ops))
  (let cc (car bt)
  (let _ _
    ((((((machine (cadr cc)) (cddddr cc)) fns) (cdr cc)) (cadddr cc)) (caddr cc))))
(if (eq? 'LDT (car ops))
((((((machine (cons (cons (cons 'try (cadr ops)) env) s)) d) fns) bt) (cddr ops)) env)
(if (eq? 'TRY_ (car ops))
  (let _ _
  (let _ _
  (((caar bt) env) bt))) ;Should be "(car bt)"
(if (eq? 'FAIL_ (car ops))
    ((((((machine s) d) fns) (cons (cdar bt) bt)) '(TRY_)) env) ;Should be simply "(cdr bt)""

(if (eq? 'AP_ (car ops))
  (if (code? (car s))
    (if lifting?
    (let s0 ((car s) (car (cadr s)))
    (let d (cons (cddr s) (cons env (cons (cdr ops) d)))
      ((((((machine (cons s0 (car d))) (cdddr d)) fns) bt) (caddr d)) (cadr d))))
    (let fun (caar s)
    (let s (cons (cdr (car s)) (cdr s))
    (let s0 (fun (mla (cons (cadr s) (cdr (car s)))))
    (let d (cons (cddr s) (cons env (cons (cdr ops) d)))
      ((((((machine (cons s0 (car d))) (cdddr d)) fns) bt) (caddr d)) (cadr d)))))))
  (if (pair? (car s))
    (if (lambda? (caar s))
      (let fun (caar s)
      (let s (cons (cdr (car s)) (cdr s))
          ;((((((machine '()) (cons (cddr s) (cons env (cons (cdr ops) d)))) fns) bt) (caar s)) (cons (cadr s) (cdr (car s))))
          (let s0 (fun (mla (cons (cadr s) (cdr (car s)))))
            (let d (cons (cddr s) (cons env (cons (cdr ops) d)))
            ((((((machine (cons s0 (car d))) (cdddr d)) fns) bt) (caddr d)) (cadr d))))))
    (if (eq? 'try (caaar s))
        (let _ _ ;(log 0 (cons 'FROMPREWRAP (caaar s)))
        (let newDump (cons (cddr s) (cons env (cons (cdr ops) (cons fns d))))
        (let wrapInstrs (lambda wrapInstrs xs
          (if (null? (cdr xs))
            (car xs)
            (cons (maybe-lift (lambda _ env (lambda _ bt ((((((machine '()) newDump) fns) bt) (cadr xs)) env))))
                  (wrapInstrs (cddr xs)))))
        (let wrapped (wrapInstrs (cdr (caar s)))
        (let _ _ ;(log 0 'GOTTOWRAP)
         ((((((machine '()) newDump) fns) (cons wrapped bt)) '(TRY_)) env)))))) ;Should merge wrapped and bt instead of pushing cons
    'ERROR))
  (if (lambda? (car s))
    (let s1 ((car s) 1)
    (let s0 ((car s1) (mla (cons (cadr s) (cdr s1))))
      (let d (cons (cddr s) (cons env (cons (cdr ops) d)))
      ((((((machine (cons s0 (car d))) (cdddr d)) fns) bt) (caddr d)) (cadr d)))))
  (let cc (car s) ;;; Purely for the CC instr
    (let newStack (cons (cadr s) (cadr cc))
    (let newEnv (caddr cc)
    (let newOps (cadddr cc)
    (let newDump (cadddr (cdr cc))
      ((((((machine newStack) newDump) fns) bt) newOps) newEnv))))))
  )))

(maybe-lift (cons 'ERROR ops))
)))))))))))))))))))))))))))))))))))))))))))))))))))))
(lambda _ ops (maybe-lift (((((machine '()) '()) '()) '()) ops))))))))))))))))))))
"""
  val evl = s"(let lifting? 0 (let possible-lift (lambda _ e e) (let maybe-lift (lambda _ e e) $src)))"
  val cmp = s"(let lifting? 0 (let possible-lift (lambda _ e e) (let maybe-lift (lambda _ e (lift e)) $src)))"
  val evg = s"(let lifting? 1 (let possible-lift (lambda _ e (lift e)) (let maybe-lift (lambda _ e e) $src)))"

  def runVM(vmSrc: String, src: String, env: String, runCopmiled: Boolean = true) = {
      if(runCopmiled)
          ev(s"((run 0 ($vmSrc $src)) (lift $env))")
      else
          ev(s"(($vmSrc $src) (lift $env))")
  }

  def test() = {
    println("// ------- SECD.test --------")
    stBlock = Nil;
    stFresh = 0;

    val prog1 = "'(LDC 10 LDC 15 ADD WRITEC)"
    val prog2 = "'(LDC 10 LDC 15 LD (1 1) ADD WRITEC)"
    val arg2 = "'((5 6 7 8 9))"
    val factorialProg = """'(NIL LDC 1 CONS LDC 10 CONS LDF
                            (DUM NIL LDF
                            (LDC 0 LD (1 1) EQ SEL
                            (LD (1 2) JOIN)
                            (NIL LD (1 2) LD (1 1) MPY CONS
                            LD (3 2) LD (1 1) SUB CONS LDR (1 1) AP JOIN)
                            RTN)
                            CONS LDF
                            (NIL LD (2 2) CONS LD (2 1) CONS LDR (1 1) AP RTN) RAP
                            RTN) AP WRITEC)"""

    check(ev(s"(($evl $prog1) '())"))("Cst(25)")

    println(reifyc(ev(s"(($cmp $prog1) (lift '()))")))
    check(ev(s"((run 0 ($cmp $prog1)) '())"))("Cst(25)")

    check(ev(s"(($evl $prog2) $arg2)"))("Cst(20)")

    check(ev(s"((run 0 ($cmp $prog2)) $arg2)"))("Cst(20)")

    println(reifyc(ev(s"""($cmp '(NIL LDC 135 CONS
                                    LDF (LDC 10 LD (1 1) ADD RTN)
                                    AP
                                    STOP))""")))

    println("evaluating factorial")
    check(ev(s"(($evl $factorialProg) '())"))("Cst(3628800)")

    println("compiling factorial")
    println(reifyc(ev(s"(($cmp $factorialProg) (lift '()))")))
    println("running...")
    check(ev(s"((run 0 ($cmp $factorialProg)) '())"))("Cst(3628800)")

    println(prettycode(reifyc(ev(s"(($cmp $factorialProg) (lift '()))"))))

    // println(prettycode(PinkBase.fac_exp_anf)) // Factorial without SECD for comparison

    println("compiling and running ackerman...")
    val ack_prog = """'(
      DUM NIL LDF
          (LDC 0 LD (1 1) EQ SEL
                  (LDC 1 LD (1 2) ADD JOIN ) (LDC 0 LD (1 2) EQ SEL
          (NIL LDC 1 CONS LDC 1 LD (1 1) SUB CONS LDR (1 1) AP JOIN ) (NIL NIL LDC 1 LD (1 2) SUB CONS LD (1 1) CONS LDR (1 1) AP CONS LDC 1 LD (1 1) SUB CONS LDR (1 1) AP JOIN ) JOIN ) RTN ) CONS LDF
          (NIL LD (2 1) CONS LD (2 2) CONS LDR (1 1) AP RTN ) RAP WRITEC
    )"""
    check(ev(s"((run 0 ($cmp $ack_prog)) '((2 2)))"))("Cst(7)")
    println(prettycode(reifyc(ev(s"(($cmp $ack_prog) (lift '()))"))))

    val ap0 = """'(
      LDF (LD (1 1) LD (1 2) ADD RTN) AP0 STOP
    )"""
    check(ev(s"(($evl $ap0) '((2 2)))"))("Tup(Cst(4),Str(.))")
    check(ev(s"((run 0 ($cmp $ap0)) '((2 2)))"))("Tup(Cst(4),Str(.))")

    val tryFail = """'(
      LDC 3 LDF (TRY (LDC 4 RTN)
                 TRY (LDC 3 RTN)
                 TRY (LDC 2 RTN)
                 TRY (LDC 1 RTN)
                 TRY (LDC 0 RTN)
                 TRY (LDC -1 RTN)
                 FAIL)
      AP0 GT SEL (FAIL JOIN) (LDC done JOIN) WRITEC
    )"""
    check(ev(s"(($evl $tryFail) '())"))("Str(done)")
    // check(ev(s"($cmp $tryFail)"))("") // TODO: @crash because staging of try/fail is not yet implemented

    val ldtTest = """'(
      NIL LDT
         (TRY_ (LDC 0 RTN)
          TRY_ (LDC 2 RTN)
          TRY_ (LDC 3 RTN) FAIL_ )
            CONS LDF (LDC 2 NIL LD (1 1) AP_ LT SEL
                 (LDC 1 JOIN )
                 (FAIL_ JOIN ) RTN ) AP_ WRITEC
    )"""

    check(ev(s"(($evl $ldtTest) '())"))("Cst(1)")
    // check(ev(s"(($cmp $ldtTest) '())"))("Cst(1)")

    testDone()
  }
}
