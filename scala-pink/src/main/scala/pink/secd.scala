import Pink._
import Lisp._
import Base._

object SECD_Machine {
  val src = """
(let locate (lambda locate i (lambda _ j (lambda _ env
(let loc (lambda loc y (lambda _ lst
(if (eq? y 1) (car lst) ((loc (- y 1)) (cdr lst)))))
((loc j) ((loc i) env))
))))
(let append (lambda append xs (lambda _ ys
  (if (eq?  '() xs) ys (cons (car xs) ((append (cdr xs)) ys)))))
(let snoc (lambda snoc x (lambda _ xs ((append xs) (cons x '()))))
(let machine (lambda machine s (lambda _ e (lambda _ c (lambda _ d
(let _ (log 0 c)
(if (eq? 'NIL (car c)) ((((machine (cons '() s)) e) (cdr c)) d)
(if (eq? 'LDC (car c)) ((((machine (cons (cadr c) s)) e) (cddr c)) d)
(if (eq? 'LD (car c))
  (let ij (cadr c) (let i (car ij) (let j (cadr ij)
  ((((machine (cons (((locate i) j) e) s)) e) (cddr c)) d))))
(if (eq? 'ADD (car c)) ((((machine (cons (+ (car s) (cadr s)) (cddr s))) e) (cdr c)) d)
(if (eq? 'SUB (car c)) ((((machine (cons (- (car s) (cadr s)) (cddr s))) e) (cdr c)) d)
(if (eq? 'MPY (car c)) ((((machine (cons (* (car s) (cadr s)) (cddr s))) e) (cdr c)) d)
(if (eq? 'CONS (car c)) ((((machine (cons (cons (car s) (cadr s)) (cddr s))) e) (cdr c)) d)
(if (eq? 'SEL (car c))
  (if (car s)
    ((((machine (cdr s)) e) (cadr c))  (cons (cdddr c) d))
    ((((machine (cdr s)) e) (caddr c)) (cons (cdddr c) d)))
(if (eq? 'JOIN (car c)) ((((machine s) e) (car d)) (cdr d))
(if (eq? 'LDF (car c))
  (let f (cadr c)
  (let fun (lambda fun xs ((((machine '()) (cons ((snoc fun) xs) e)) f) '()))
  ((((machine (cons fun s)) e) (cddr c)) d)))
(if (eq? 'AP (car c))
  (let fun (car s)
  (let vs (cadr s)
  (let r (fun vs)
  ((((machine (cons r s)) e) (cdr c)) d))))
(if (eq? 'RTN (car c))
  (car s)
(if (eq? 'STOP (car c)) s
(if (eq? 'WRITEC (car c)) (car s)
(cons 'ERROR c))))))))))))))))))))
(lambda _ c ((((machine '()) '()) c) '()))))))
"""

  val evl = src

  def test() = {
    println("// ------- SECD_Machine.test --------")

    check(ev(s"($evl '(LDC 1 WRITEC))"))("Cst(1)")
    check(ev(s"($evl '(LDC 1 LDC 2 ADD WRITEC))"))("Cst(3)")
    check(ev(s"($evl '(LDC 2 SEL (LDC 1 JOIN) (LDC 0 JOIN) WRITEC))"))("Cst(1)")
    check(ev(s"($evl '(NIL LDC 2 CONS LDC 1 CONS LDF (LDC 2 LDC 1 ADD RTN) AP WRITEC))"))("Cst(3)")
    check(ev(s"($evl '(NIL LDC 2 CONS LDC 1 CONS LDF (LD (1 2) LD (1 1) ADD RTN) AP WRITEC))"))("Cst(3)")
    check(ev(s"($evl '(NIL LDC 6 CONS LDF (LD (1 1) SEL (NIL LDC 1 LD (1 1) SUB CONS LD (1 2) AP LD (1 1) MPY JOIN) (LDC 1 JOIN) RTN) AP WRITEC))"))("Cst(720)")
  }
}
