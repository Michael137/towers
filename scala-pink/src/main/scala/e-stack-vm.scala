// User-level VM
// Summary: Based on SECD/CESK
// Scheme implementation in scheme-pink/stack-vm.scm

import EBase._
import ELisp._

object EVM {
    val vm_poly_src = """
    (let dbgCtr 12
        (let vm-stack '()
            (let env-list '()
                (let op-list '()
                    (let call-stack '()
                        (let locate (lambda (i j env)
                                    (letrec ((loc (lambda (y lst) (if (eq? y 1) (car_ lst) (loc (- y 1) (cdr_ lst))))))
                                            (loc j (loc i env))
                        ))
                        (letrec ((machine (lambda (s e c d ops)
                                            (if (eq? 'STOP (car_ ops)) s
                                            (if (eq? 'LDC (car_ ops)) (machine (cons_ (cadr_ ops) s) e c d (cddr_ ops))
                                            (if (eq? 'LD (car_ ops))
                                                (machine (cons_ (locate (car_ (cadr_ ops)) (cadr_ (cadr_ ops)) e) s) e c d (cddr_ ops))
                                            (if (eq? 'ADD (car_ ops)) (machine (cons_ (+ (car_ s) (cadr_ s)) (cddr_ s)) e c d (cdr_ ops))
                                            (if (eq? 'SUB (car_ ops)) (machine (cons_ (- (car_ s) (cadr_ s)) (cddr_ s)) e c d (cdr_ ops))
                                            (if (eq? 'MPY (car_ ops)) (machine (cons_ (* (car_ s) (cadr_ s)) (cddr_ s)) e c d (cdr_ ops))
                                            (if (eq? 'EQ (car_ ops)) (machine (cons_ (eq? (ref (car_ s)) (ref (cadr_ s))) (cddr_ s)) e c d (cdr_ ops))
                                            (if (eq? 'GT (car_ ops)) (machine (cons_ (> (car_ s) (cadr_ ops)) (cdr_ s)) e c d (cddr_ ops))
                                            (if (eq? 'CONS (car_ ops)) (machine (cons_ (cons_ (car_ s) (cadr_ s)) (cddr_ s)) e c d (cdr_ ops))
                                            (if (eq? 'NIL (car_ ops)) (machine (cons_ '() s) e c d (cdr_ ops))      
                                            (if (eq? 'SEL (car_ ops))
                                                (if (eq? (ref (car_ s)) 0)
                                                    (machine (cdr_ s) e c (cons_ (cdddr_ ops) d) (cadr_ ops))
                                                    (machine (cdr_ s) e c (cons_ (cdddr_ ops) d) (caddr_ ops)))
                                            (if (eq? 'JOIN (car_ ops))
                                                (machine s e c (cdr_ d) (ref (car_ d)))
                                            (if (eq? 'LDF (car_ ops)) (machine (cons_ (cons_ (cadr_ ops) e) s) e (cons_ (cadr_ ops) c) d (cdr_ ops))
                                            (if (eq? 'AP (car_ ops))
                                                (machine '() (cons_ (cadr_ s) (cdr_ (car_ s))) c (cons_ (cddr_ s) (cons_ e (cons_ (cdr_ ops) d))) (ref (caar_ s)))
                                            (if (eq? 'RTN (car_ ops))
                                                (let resume (ref (caddr_ d))
                                                    (machine (cons_ (car_ s) (car_ d)) (cadr_ d) c (cdddr_ d) resume))
                                            (if (eq? 'DUM (car_ ops)) (machine s (cons_ '() e) c d (cdr_ ops))
                                            (if (eq? 'RAP (car_ ops))
                                                (let _ (set-car!_ e (cadr_ s))
                                                    (machine '() e c (cons_ (cddr_ s) (cons_ e (cons_ (cdr_ ops) d))) (ref (caar_ s))))
                                                                      
                                            (if (eq? 'WRITEC (car_ ops)) (ref (car_ s))
                                            (if (eq? 'DBG (car_ ops)) (if (eq? dbgCtr 0) (ref (cdr_ (car_ e))) (let _ (log 0 (ref (cdr_ (car_ e)))) (let _ (set! dbgCtr (- dbgCtr 1)) (machine s e c d (cdr_ ops)))))
                                            (if (eq? 'DBG2 (car_ ops)) (ref (car_ s))
                                                (machine s e c d (cdr_ ops)))))))))))))))))))))))))
                            (let start (lambda (ops) (machine vm-stack env-list op-list call-stack ops))
                                        (start 
                                        '(
                                            NIL LDC 1 CONS LDC 10 CONS LDF
                                                (DUM NIL LDF
                                                    (LDC 0 LD (1 1) EQ SEL
                                                        (NIL LD (1 2) LD (1 1) MPY CONS
                                                                LD (3 2) LD (1 1) SUB CONS LD (2 1) AP JOIN)
                                                        (LD (1 2) JOIN) ;this line deviates from book but is correct for now
                                                    RTN)
                                                CONS LDF
                                                    (NIL LD (2 2) CONS LD (2 1) CONS LD (1 1) AP RTN) RAP
                                                RTN) AP STOP
                                        ))
        ))))))))
    """

  val vm_src = s"(let maybe-lift (lambda (e) e) $vm_poly_src)"
  val vmc_src = s"(let maybe-lift (lambda (e) (lift e)) $vm_poly_src)"

  def test() = {
    println("// ------- EVM.test --------")
    println(deref(inject(trans(parseExp(vm_src), Nil))))
  }
}

import VM._