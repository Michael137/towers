// User-level VM
// Summary: Based on SECD/CESK
// Scheme implementation in scheme-pink/stack-vm.scm

import Base._
import Lisp._
import PinkBase._
import Pink._

/*
 * S-register: vm-stack
 * E-register: env-list
 * C-register: op-list
 * D-register: call-stack
 */
object VM {
  val vm_poly_src = """
      (let vm-stack '()
        (let env-list '()
          (let op-list '()
            (let call-stack '()
              (let locate (lambda locate i (lambda _ j (lambda _ env
                            (let loc (lambda loc y (lambda _ lst
                                      (if (eq? y 1) (car lst) ((loc (- y 1)) (cdr lst)))
                                    ))
                            ((loc j) ((loc i) env)))
                          )))
              (let machine (lambda machine s (lambda _ e (lambda _ c (lambda _ d (lambda _ ops
                                (if (eq? 'LDC (car ops)) (((((machine (cons (cadr ops) s)) e) (cddr ops)) d) (cddr ops))
                                (if (eq? 'LD (car ops))
                                  (let pt (cadr ops)
                                    (((((machine (cons (((locate (car pt)) (cadr pt)) e) s)) e) (cddr ops)) d) (cddr ops)))
                                (if (eq? 'ADD (car ops)) (((((machine (cons (+ (car s) (cadr s)) (cddr s))) e) (cdr ops)) d) (cdr ops))
                                (if (eq? 'SUB (car ops)) (((((machine (cons (- (car s) (cadr s)) (cddr s))) e) (cdr ops)) d) (cdr ops))
                                (if (eq? 'MPY (car ops)) (((((machine (cons (* (car s) (cadr s)) (cddr s))) e) (cdr ops)) d) (cdr ops))
                                (if (eq? 'CONS (car ops)) (((((machine (cons (cons (car s) (cadr s)) (cddr s))) e) (cdr ops)) d) (cdr ops))
                                (if (eq? 'NIL (car ops)) (((((machine (cons '() s)) e) (cdr ops)) d) (cdr ops))
                                (if (eq? 'SEL (car ops))
                                  (let next
                                    (if (eq? (car s) 0)
                                      (caddr ops)
                                      (cadr ops))
                                  (((((machine s) e) next) (cons (cdddr ops) d)) next))
                                (if (eq? 'JOIN (car ops))
                                  (let rest
                                    (car d)
                                  (((((machine s) e) rest) (cdr d)) rest))
                                (if (eq? 'LDF (car ops)) (((((machine (cons (cons (cadr ops) e) s)) e) (cdr ops)) d) (cdr ops))
                                (if (eq? 'AP (car ops))
                                 (((((machine '()) (cons (cadr s) (cdr (car s)))) (caar s)) (cons (cddr s) (cons e (cons (cdr ops) d)))) (caar s))
                                (if (eq? 'RTN (car ops))
                                  (let resume (caddr d)
                                    (((((machine (cons (car s) (car d))) (cadr d)) resume) (cdddr d)) resume))
                                (if (eq? 'DUM (car ops)) (((((machine s) (cons '() e)) (cdr ops)) d) (cdr ops))
                                (if (eq? 'RAP (car ops)) (((((machine '()) (cons (cddr (car s)) (cadr s))) (caar s)) (cons (cddr s) (cons (cdr e) (cons (cdr ops) d)))) (caar s))
                                (if (eq? 'STOP (car ops)) s

                                (if (eq? 'DBG (car ops))
                                  (caddr d)
                                  
                                (((((machine s) e) c) d) (cdr ops))))))))))))))))))
                                )))))
              (let start (lambda start ops
                            (((((machine vm-stack) env-list) op-list) call-stack) ops)
                          )
                          start
              )))
      ))))
    """

  // Example: ev(s"((($eval_src '$vm_src) '(PUSH 10)) '(POP))")

  val vm_src = s"(let maybe-lift (lambda _  e e) $vm_poly_src)"
  val vmc_src = s"(let maybe-lift (lambda _  e (lift e)) $vm_poly_src)"

  // TODO: tracing stack

  def test() = {
    println("// ------- VM.test --------")

    // val vm_exp = trans(parseExp(vm_src), Nil)
    // println( "base-lang AST: " + vm_exp)
    // println( "eval'ed base-lang AST: " + evalms(Nil, vm_exp))
    // val vm_anf = reify { anf(List(Sym("XX")),vm_exp) }
    // println(prettycode(vm_anf))

    println(ev(s"""($vm_src '(LDC -10
                              LDC 10
                              ADD
                              SEL (LDC 20 JOIN) (LDC 30 JOIN)
                              NIL LDC 136 CONS LDC 1 CONS
                              LDF (LD (1 2) LD (1 1) ADD RTN)
                              AP
                              LDC 137
                              STOP))"""))

    println(ev(s"""
    ($vm_src
  '(NIL LDC 6 CONS LDF  
                   (NIL LDC 5 CONS LDC 3 CONS 
                     LDF 
                      (LD (2 1) LD (1 2) LD (1 1) SUB ADD RTN) 
                     AP 
                    RTN) 
                  AP STOP
  ))
    """))

    // Factorial
    /*println(ev(s"""($vm_src '(
      NIL LDC 1 CONS LDC 3 CONS LDF
        (DUM NIL LDF
          (LDC 0 LD (1 1) EQ SEL
              (LDC 1 JOIN)
              (NIL LD (1 2) LD (1 1) MPY CONS
                LD (3 2) LD (1 1) SUB CONS LD (2 1) AP JOIN)
              RTN)
      CONS LDF
        (NIL LD (2 2) CONS LD (2 1) CONS LD (1 1) AP RTN)
        RAP RTN)
      AP STOP
    ))"""))*/

    testDone()
  }
}

import VM._