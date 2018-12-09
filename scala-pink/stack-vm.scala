// User-level VM
// Summary: interprets SECD ISA
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
          (let locate (lambda locate idx (lambda _ env
                            (let loc (lambda loc y (lambda _ lst
                                      (if (eq? y 1) (car lst) ((loc (- y 1)) (cdr lst)))
                                    ))
                            ((loc (cdr idx)) ((loc (car idx)) env)))
                      ))
          (let machine (lambda machine s (lambda _ e (lambda _ c (lambda _ d (lambda _ ops
                            (if (eq? 'LDC (car ops)) (((((machine (cons (cadr ops) s)) e) (cdr ops)) d) (cdr ops))
                            (if (eq? 'LD (car ops)) (((((machine (cons ((locate (cadr ops)) e) s)) e) (cdr ops)) d) (cdr ops))
                            (((((machine s) e) c) d) (cdr ops)))
                        ))))))
              ((((machine vm-stack) env-list) op-list) call-stack)
          ))
  ))))
    """

  // Example: ev(s"((($eval_src '$vm_src) '(PUSH 10)) '(POP))")

  val vm_src = s"(let maybe-lift (lambda _  e e) $vm_poly_src)"
  val vmc_src = s"(let maybe-lift (lambda _  e (lift e)) $vm_poly_src)"

  // TODO: tracing stack

  def test() = {
    println("// ------- VM.test --------")

    println( "base-lang AST: " + trans(parseExp(vm_src), Nil))
    println( "eval'ed base-lang AST: " + evalms(Nil, trans(parseExp(vm_src), Nil)))
    // ev(s"(((($eval_src '$vm_src) '(PUSH 10)) '(POP)) '(PUSH 30))")
    // ev(s"(((((($vm_src '(PUSH 0)) '(PUSH 23)) '(PUSH 42)) '(POP)) '(PUSH 137)) '(DONE))")

    testDone()
  }
}

import VM._