// User-level VM
// Summary: Based on SECD/CESK
// Scheme implementation in scheme-pink/stack-vm.scm

import EBase._
import ELisp._

object EVM {
  val vm_poly_src = """
        (let vm-stack '()
            (let env-list '()
                (let op-list '()
                    (let call-stack '()
                        (let locate (lambda (i j env)
                                    (letrec ((loc (lambda (y lst) (if (eq? y 1) (car lst) (loc (- y 1) (cdr lst))))))
                                            (loc j (loc i env))
                        ))
                        (letrec ((machine (lambda (s e c d ops)
                                            (if (eq? 'STOP (car ops)) s
                                            (if (eq? 'LDC (car ops)) (machine (cons (cadr ops) s) e c d (cddr ops))
                                            (if (eq? 'LD (car ops))
                                                (machine (cons (locate (car (cadr ops)) (cadr (cadr ops)) e) s) e c d (cddr ops))
                                            (if (eq? 'ADD (car ops)) (machine (cons (+ (car s) (cadr s)) (cddr s)) e c d (cdr ops))
                                            (if (eq? 'SUB (car ops)) (machine (cons (- (car s) (cadr s)) (cddr s)) e c d (cdr ops))
                                            (if (eq? 'MPY (car ops)) (machine (cons (* (car s) (cadr s)) (cddr s)) e c d (cdr ops))
                                            (if (eq? 'EQ (car ops)) (machine (cons (eq? (car s) (cadr s)) (cddr s)) e c d (cdr ops))
                                            (if (eq? 'GT (car ops)) (machine (cons (> (car s) (cadr ops)) (cdr s)) e c d (cddr ops))
                                            (if (eq? 'CONS (car ops)) (machine (cons (cons (car s) (cadr s)) (cddr s)) e c d (cdr ops))
                                            (if (eq? 'NIL (car ops)) (machine (cons '() s) e c d (cdr ops))      
                                            (if (eq? 'SEL (car ops))
                                                (if (eq? (car s) (maybe-lift 0))
                                                    (machine (cdr s) e c (cons (cdddr ops) d) (caddr ops))
                                                    (machine (cdr s) e c (cons (cdddr ops) d) (cadr ops)))
                                            (if (eq? 'JOIN (car ops))
                                                (machine s e c (cdr d) (car d))
                                            (if (eq? 'LDF (car ops)) (machine (cons (cons (cadr ops) e) s) e (cons (cadr ops) c) d (cdr ops))
                                            (if (eq? 'AP (car ops))
                                                (machine '() (cons (cadr s) (cdr (car s))) c (cons (cddr s) (cons e (cons (cdr ops) d))) (caar s))
                                            (if (eq? 'RTN (car ops))
                                                (let resume (caddr d)
                                                    (machine (cons (car s) (car d)) (cadr d) c (cdddr d) resume))
                                            (if (eq? 'DUM (car ops)) (machine s (cons '() e) c d (cdr ops))
                                            (if (eq? 'RAP (car ops)) (machine '() (set-car! e (cadr s)) c (cons (cddr s) (cons e (cons (cdr ops) d))) (caar s))
                                            (if (eq? 'WRITEC (car ops)) (car s)
                                            (if (eq? 'DBG (car ops)) d
                                                (machine s e c d (cdr ops))))))))))))))))))))))))
                            (let start (lambda (ops) (machine vm-stack env-list op-list call-stack ops))
                                        (start 
                                        '(
                                            STOP
                                        ))
        )))))))
    """
/*
NIL LDC 1 CONS LDC 3 CONS LDF
                                            (DUM NIL LDF
                                                (DBG LDC 0 LD (1 1) EQ SEL
                                                (LDC 1 JOIN)
                                                (NIL LD (1 2) LD (1 1) MPY CONS
                                                    LD (3 2) LD (1 1) SUB CONS LD (2 1) AP JOIN)
                                                RTN)
                                            CONS LDF
                                                (DBG NIL LD (2 2) CONS LD (2 1) CONS LD (1 1) AP RTN) RAP
                                            RTN) AP
                                            */
  val vm_src = s"(let maybe-lift (lambda (e) e) $vm_poly_src)"
  val vmc_src = s"(let maybe-lift (lambda (e) (lift e)) $vm_poly_src)"

  def test() = {
    println("// ------- EVM.test --------")
    
    println(inject(trans(parseExp(vm_src), Nil)))
  }
}

import VM._