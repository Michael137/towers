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
 *
 * Instructions:
 *  Core instructions based on original Landin SECD machine
 * 
 * Extensions:
 *  DUP: duplicate top element on stack; (x.s) -> x.(x.s)
 *  NEG: negate top element of stack; (x.s) -> (mul x -1).s
 *  REP: repeat most recent LDF function (called from within body of function)
 *  PUSHENV: push operand onto env-list
 *  SUBENV: SUB but on env-list instead of stack
 *  NEGENV: NEG but on env-list instead of stack
 *  DUPENV: DUP but on env-list instead of stack
 *  PAP: persistent AP; same as AP but keeps stack intact
 *  DBG: change definition as needed;
 *       useful in debugging E.g. change def to "(car s)" will 
 *       stop evluation at DBG point and print front top of current stack
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
                                (if (eq? (maybe-lift 'LDC) (car ops)) (((((machine (cons (cadr ops) s)) e) c) d) (cddr ops))
                                (if (eq? (maybe-lift 'LD) (car ops))
                                  (let pt (cadr ops)
                                    (((((machine (cons (((locate (car pt)) (cadr pt)) e) s)) e) c) d) (cddr ops)))
                                (if (eq? (maybe-lift 'ADD) (car ops)) (((((machine (cons (+ (car s) (cadr s)) (cddr s))) e) c) d) (cdr ops))
                                (if (eq? (maybe-lift 'SUB) (car ops)) (((((machine (cons (- (car s) (cadr s)) (cddr s))) e) c) d) (cdr ops))
                                (if (eq? (maybe-lift 'MPY) (car ops)) (((((machine (cons (* (car s) (cadr s)) (cddr s))) e) c) d) (cdr ops))
                                (if (eq? (maybe-lift 'EQ) (car ops)) (((((machine (cons (eq? (car s) (cadr s)) (cddr s))) e) c) d) (cdr ops))
                                (if (eq? (maybe-lift 'GT) (car ops)) (((((machine (cons (> (car s) (cadr ops)) (cdr s))) e) c) d) (cddr ops))
                                (if (eq? (maybe-lift 'CONS) (car ops)) (((((machine (cons (cons (car s) (cadr s)) (cddr s))) e) c) d) (cdr ops))
                                (if (eq? (maybe-lift 'NIL) (car ops)) (((((machine (cons '() s)) e) c) d) (cdr ops))
                                (if (eq? (maybe-lift 'SEL) (car ops))
                                  (let next
                                    (if (eq? (car s) 0)
                                      (caddr ops)
                                      (cadr ops))
                                  (((((machine (cdr s)) e) c) (cons (cdddr ops) d)) next))
                                (if (eq? 'JOIN (car ops))
                                  (let rest
                                    (car d)
                                  (((((machine s) e) c) (cdr d)) rest))
                                (if (eq? 'LDF (car ops)) (((((machine (cons (cons (cadr ops) e) s)) e) (cons (cadr ops) c)) d) (cdr ops))
                                (if (eq? 'AP (car ops))
                                 (((((machine '()) (cons (cadr s) (cdr (car s)))) c) (cons (cddr s) (cons e (cons (cdr ops) d)))) (caar s))
                                (if (eq? 'RTN (car ops))
                                  (let resume (caddr d)
                                    (((((machine (cons (car s) (car d))) (cadr d)) c) (cdddr d)) resume))
                                (if (eq? 'DUM (car ops)) (((((machine s) (cons '() e)) c) d) (cdr ops))
                                (if (eq? 'RAP (car ops)) (((((machine '()) (cons (cddr (car s)) (cadr s))) c) (cons (cddr s) (cons (cdr e) (cons (cdr ops) d)))) (caar s))
                                (if (eq? 'WRITEC (car ops)) (car s)
                                (if (eq? 'STOP (car ops)) s

                                (if (eq? 'DUP (car ops)) (((((machine (cons (car s) s)) e) c) d) (cdr ops))
                                (if (eq? 'REP (car ops)) (((((machine s) e) c) d) (car c))
                                (if (eq? 'NEG (car ops)) (((((machine (cons (* (car s) -1) (cdr s))) e) c) d) (cdr ops))
                                (if (eq? 'PUSHENV (car ops)) (((((machine s) (cons (cons (cadr ops) (car e)) (cdr e))) c) d) (cddr ops))
                                (if (eq? 'SUBENV (car ops)) (((((machine s) (cons (cons (- (caar e) (cadr (car e))) (cddr (car e)))
                                                                                  (cdr e))) c) d) (cdr ops))
                                (if (eq? 'NEGENV (car ops)) (((((machine s) (cons (cons (* (caar e) -1) (cdr (car e)))
                                                                                  (cdr e))) c) d) (cdr ops))
                                (if (eq? 'DUPENV (car ops)) (((((machine s) (cons (cons (caar e) (car e)) (cdr e))) c) d) (cdr ops))
                                (if (eq? 'PAP (car ops))
                                 (((((machine (cadr s)) (cons (cadr s) (cdr (car s)))) c) (cons (cddr s) (cons e (cons (cdr ops) d)))) (caar s))
                                (if (eq? 'DBG (car ops))
                                  e
                                  
                                (((((machine s) e) c) d) (cdr ops)))))))))))))))))))))))))))))
                                )))))
              (let start (lambda start ops
                            (((((machine vm-stack) env-list) op-list) call-stack) ops)
                          )
                          start
              )))
      ))))
    """

  val vm_src = s"(let maybe-lift (lambda _  e e) $vm_poly_src)"
  val vmc_src = s"(let maybe-lift (lambda _  e (lift e)) $vm_poly_src)"

  def testInstructions() = {
    checkrun(s"""($vm_src '(LDC -10 ; Comments work fine
                              LDC 10 ; As in Lisp
                              ADD
                              SEL (LDC 20 JOIN) (LDC 30 JOIN)
                              NIL LDC 136 CONS LDC 1 CONS
                              LDF (LD (1 2) LD (1 1) ADD RTN)
                              AP
                              LDC 137
                              STOP))""", "Tup(Cst(137),Tup(Cst(137),Tup(Cst(30),Str(.))))")

    // ((λx.λyz.(y-z)+x) 6) 3 5) = 3 - 5 + 6
    checkrun(s"""
      ($vm_src
      '(NIL LDC 6 CONS LDF  
                      (NIL LDC 5 CONS LDC 3 CONS 
                        LDF 
                          (LD (2 1) LD (1 2) LD (1 1) SUB ADD RTN) 
                        AP 
                        RTN) 
                      AP STOP
      ))
    """, "Tup(Cst(4),Str(.))")

    // ((λuv.λwx.λyz.(y-x)+u) 2 1) 4 3) 6 5 = 6 - 3 + 2
    checkrun(s"""
      ($vm_src
      '(NIL LDC 1 CONS LDC 2 CONS LDF  
                      (NIL LDC 3 CONS LDC 4 CONS 
                        LDF 
                          (NIL LDC 5 CONS LDC 6 CONS 
                          LDF (LD (3 1) LD (2 2) LD (1 1) SUB ADD RTN) AP RTN)
                        AP 
                        RTN) 
                      AP STOP
      ))
    """, "Tup(Cst(5),Str(.))")

    checkrun(s"""($vm_src '(NIL ; Equivalent to calling f() without arguments
                                  ; Have to place '() onto stack
                              LDF (LDC 136 RTN)
                              AP
                              LDC 1
                              ADD
                              STOP))""", "Tup(Cst(137),Str(.))")

    checkrun(s"""($vm_src '(
      NIL LDC 1 CONS LDC 10 CONS LDF
        (DUM NIL LDF (LDC 1 RTN) AP RTN)
      AP STOP
    ))""", "Tup(Cst(1),Str(.))")
  }

  def getFacSource(n: Int) = {
    // Factorial
    // ? Should use RAP instead
    s"""
        NIL LDC $n CONS ; Maximum Scala int can handle is n = 25
        LDF
        (DUPENV
        LD (1 1)
        LDC 1
        SUB
        NEG
        MPY

        PUSHENV 1
        SUBENV
        NEGENV

        LD (1 1)
        GT 1
        SEL (REP) (WRITEC)
        RTN) PAP
    """
  }

  def testFactorial() = {
    checkrun(s"""($vm_src '(${getFacSource(25)}))""", "Cst(2076180480)")
    //checkrun(s"((run 0 ($vmc_src)) $fac_src)", "Cst(2076180480)")

    // interpretation
    checkrun(s"""
    (let vm $vm_src
      (let fac_src '(${getFacSource(4)})
        (vm fac_src)))""", "Cst(24)")

    // compilation
    val factorial_val = parseExp(s"""'(${getFacSource(4)})""")
    val factorial_exp = trans(fac_val,List("arg"))
    val factorial_exp_anf = reify { anf(List(Sym("XX")),factorial_exp) }
    checkcode(s"""
    (let vm_compiled $vmc_src
      (let fac_src '(${getFacSource(4)})
        (vm_compiled fac_src)))""",
    prettycode(fac_exp_anf))

    /*
    checkrun(s"""
    (let evalc         $evalc_src
    (let fac_src       (quote $fac_src)

    ((run 0 (evalc fac_src)) 4)))""",
    "Cst(24)")

    // optimality: verify collapse
    checkcode(s"""
    (let eval          $eval_src
    (let evalc_src     (quote $evalc_src)
    (let fac_src       (quote $fac_src)

    ((eval evalc_src) fac_src))))""",
    prettycode(fac_exp_anf))

    checkcode(s"""
    (let eval          $eval_src
    (let evalc_src     (quote $evalc_src)
    (let eval_src      (quote $eval_src)

    ((eval evalc_src) eval_src))))""",
    prettycode(eval_exp_anf))

    checkcode(s"""
    (let eval          $eval_src
    (let evalc_src     (quote $evalc_src)

    ((eval evalc_src) evalc_src)))""",
    prettycode(evalc_exp_anf))

    // further tower
    checkcode(s"""
    (let eval          $eval_src
    (let eval_src      (quote $eval_src)
    (let evalc_src     (quote $evalc_src)
    (let fac_src       (quote $fac_src)

    (((eval eval_src) evalc_src) fac_src)))))""",
    prettycode(fac_exp_anf))*/
  }

  def pretty() = {
    val vm_exp = trans(parseExp(vm_src), Nil)
    val vm_anf = reify { anf(List(Sym("XX")),vm_exp) }
    println(prettycode(vm_anf))
  }

  def test() = {
    println("// ------- VM.test --------")
    testFactorial()
    testInstructions()

    testDone()
  }
}

import VM._