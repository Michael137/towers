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
(let pla (lambda pla xs (if (code? xs) xs (if (pair? xs) (possible-lift (cons (pla (car xs)) (pla (cdr xs)))) (possible-lift xs))))
(let atom? (lambda atom? a (if (sym? a) (maybe-lift 1) (if (num? a) (maybe-lift 1) (maybe-lift 0))))
(let locate (lambda locate i (lambda _ j (lambda _ env
(let loc (lambda loc y (lambda _ lst
(if (eq? y 1) (car lst) ((loc (- y 1)) (cdr lst)))))
((loc j) ((loc i) env))
))))
(let machine (lambda machine s (lambda _ d (lambda _ fns (lambda _ bt (lambda _ ops (lambda _ env
(if (eq? 'LIFT (car ops))
  (let r
    (if (or (sym? (car s)) (num? (car s)))
      (lift (car s))
    (lift (lambda _ x ((((((machine '()) (cons 'ret d)) fns) bt) (car (cdr (car s)))) (cons (cons x '()) (cdr (cdar s)))))))
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
  ((((((machine (cons (((locate (car (cadr ops))) (cadr (cadr ops))) fns) s)) (cons 'ldr d)) fns) bt) (cddr ops)) env)
(if (eq? 'LDF (car ops))
  ((((((machine (cons
                  (cons (lambda _ env ((((((machine '()) (cons 'ret d)) fns) bt) (cadr ops)) env))
                        (cons (cadr ops) env))
                  s)) d) fns) bt) (cddr ops)) env)
(if (eq? 'NIL (car ops))
((((((machine (cons (maybe-lift '()) s)) d) fns) bt) (cdr ops)) env)
(if (eq? 'AP (car ops))
  (if (code? (car s))
    (if lifting?
      (let fn (car s)
      (let newEnv (car (cadr s))
        ((((((machine (cons (fn newEnv) (cddr s))) d) fns) bt) (cdr ops)) env)))
      (let fn (caar s)
        (let s (cons (cdar s) (cdr s))
          (let res (fn (mla (cons (cadr s) (cdar s))))
            ((((((machine (cons res (cddr s))) d) fns) bt) (cdr ops)) env)))))
  (if (and (pair? d)
           (eq? 'ldr (car d)))
    (let d (if (eq? 'ldr (car d))
          (cdr d)
          d)
      (let closure (car s)
      (let fn (car closure)
        (let res (fn (mla (cons (cadr s) (cddr closure))))
          ((((((machine (cons res (cddr s))) d) fns) bt) (cdr ops)) env)))))
  (if (lambda? (caar s))
    (let fn (caar s)
    (let s (cons (cdar s) (cdr s))
        (let res (fn (mla (cons (cadr s) (cdar s))))
            ((((((machine (cons res (cddr s))) d) fns) bt) (cdr ops)) env))))
  (if (eq? 'try (caaar s))
    'ERROR
  'ERROR))))

(if (eq? 'RTN (car ops))
  (if (and (pair? d)
           (eq? 'ldr (car d)))
    (if (code? (maybe-lift 1)) ;this means "RTN" was called immediately after "LDR"
      (mla
        (let closure (car s)
        (let fn (car closure)
        (cons ;This cons is not necessary but currently just there for calling convention
          (lambda _ arg (fn (mla (cons (car arg) (cddr closure)))))
          '(() ())))))
      (car s))
  (if (and (pair? d)
           (eq? 'ret (car d)))
    (mla (car s))
      ((((((machine (cons (car s) (car d))) (cddddr d)) (cadddr d)) bt) (caddr d)) (cadr d))))
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
  (let recClo (cadr s)
  (let entryClo (car s)
  (let entryOps (car (cdar s))
  (let recOpsEnv (cdar recClo)
  (let oldEnv (cdr recClo)
  (let recOpsEnvEnv (cons recOpsEnv oldEnv)
  (let recEnv (cdar recOpsEnvEnv)
  (let recOps (caar recOpsEnvEnv)
  (let newDump (cons (cddr s) (cons env (cons (cdr ops) (cons fns d))))
  (let mem '()
  (let rec (maybe-lift (lambda rec env ((((((machine '()) (cons 'ret d)) (cons (cons (cons rec (cons mem recEnv)) oldEnv) fns)) bt) recOps) env)))
      ((((((machine '()) newDump) (cons (cons (cons rec (cons mem recEnv)) oldEnv) fns)) bt) entryOps) env))))))))))))
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

(maybe-lift (cons 'ERROR ops))
)))))))))))))))))))))))))))))))))))))))))))
(lambda _ ops (maybe-lift (((((machine '()) '()) '()) '()) ops))))))))))))))))))))
"""
  val evl = s"(let lifting? 0 (let possible-lift (lambda _ e e) (let maybe-lift (lambda _ e e) $src)))"
  val cmp = s"(let lifting? 0 (let possible-lift (lambda _ e e) (let maybe-lift (lambda _ e (lift e)) $src)))"
  val evg = s"(let lifting? 1 (let possible-lift (lambda _ e (lift e)) (let maybe-lift (lambda _ e e) $src)))"

  def runVM(vmSrc: String, src: String, env: String, runCompiled: Boolean = true, liftEnv: Boolean = false) = {
      val e = if(liftEnv) s"(lift $env)" else env

      if(runCompiled)
          ev(s"((run 0 ($vmSrc $src)) $e)")
      else
          ev(s"(($vmSrc $src) $e)")
  }

  def test() = {
    println("// ------- SECD.test --------")

    SECDTests.testBasic
    SECDTests.factorialTests
    SECDTests.testAckermann
    SECDTests.testTryFail
    SECDTests.basicTests
    SECDTests.listAccessTest

    testDone()
  }
}
