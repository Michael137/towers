// User-level VM
// Summary: Based on SECD/CESK
// Scheme implementation in scheme-pink/stack-vm.scm

import EBase._
import ELisp._

object EVMUtils {
    val fac_src = """
                    NIL LDC 1 CONS LDC 10 CONS LDF
                        (DUM NIL LDF
                            (LDC 0 LD (1 1) EQ SEL
                                (LD (1 2) JOIN) ;this line deviates from book but is correct for now
                                (NIL LD (1 2) LD (1 1) MPY CONS
                                        LD (3 2) LD (1 1) SUB CONS LD (2 1) AP JOIN)
                            RTN)
                        CONS LDF
                            (NIL LD (2 2) CONS LD (2 1) CONS LD (1 1) AP RTN) RAP
                        RTN) AP WRITEC ;could be "STOP" instead
                """

    val simple_src = """
            LDC 10
            LDC 20
            ADD
            LDC 29
            EQ
            SEL (LDC 1 JOIN) (LDC -1 JOIN)
            NIL LDC 136 CONS
            LDF (LD (1 1) LDC 1 ADD RTN)
            AP
            WRITEC
        
    """
}

object EVM {
    import EVMUtils._
    import TestHelpers._

    val vm_poly_src = """
    (lambda (program)
        (let dbgCtr 8
            (let vm-stack '()
                (let env-list '()
                    (let op-list '()
                        (let call-stack '()
                            (let locate (lambda (i j env)
                                        (letrec ((loc (lambda (y lst) (if (eq? y 1) (car_ lst) (loc (- y 1) (cdr_ lst))))))
                                                (loc j (loc i env))
                            ))
                            (letrec ((machine (lambda (s e c d ops)
                                                (if (eq? 'STOP (car_ ops)) (maybe-lift s)
                                                (if (eq? 'LDC (car_ ops)) (machine (cons_ (maybe-lift (cadr_ ops)) s) e c d (cddr_ ops))
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
                                                    (maybe-lift (if (eq? (ref (car_ s)) (maybe-lift 0))
                                                        (machine (cdr_ s) e c (cons_ (cdddr_ ops) d) (caddr_ ops))
                                                        (machine (cdr_ s) e c (cons_ (cdddr_ ops) d) (cadr_ ops))))
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
                                                                        
                                                (if (eq? 'WRITEC (car_ ops)) (maybe-lift (ref (car_ s)))
                                                (if (eq? 'DBG (car_ ops)) (if (eq? dbgCtr 0) (ref (cdr_ (car_ e))) (let _ (log 0 (ref (cdr_ (car_ e)))) (let _ (set! dbgCtr (- dbgCtr 1)) (machine s e c d (cdr_ ops)))))
                                                (if (eq? 'DBG2 (car_ ops)) (car_ s)
                                                    (machine s e c d (cdr_ ops)))))))))))))))))))))))))
                                (let start (lambda (ops) (machine vm-stack env-list op-list call-stack ops))
                                            (start program)
            )))))))))
    """

    val vm_src = s"(let maybe-lift (lambda (e) e) $vm_poly_src)"
    val vmc_src = s"(let maybe-lift (lambda (e) (lift e)) $vm_poly_src)"
    
    def runVM(vmSrc: String, src: Any) = {
        val parsed = src match {
            case str: String =>
                val Tup(Str(_), Tup(ret, Str(_))) = parseExp(s"'($str)")
                ret
            case exp: Val => exp
        }

        val vmSrcExp = trans(parseExp(vmSrc), Nil)
        val vm_src_state = evalms(State(vmSrcExp, initEnv, initStore, Halt())).asInstanceOf[Answer]
        val state = applyProc(vm_src_state.v, List(parsed), vm_src_state.s, Halt()).asInstanceOf[State]
        evalms(state).asInstanceOf[Answer].v
    }

    /*
    ** We expect evaluation and compilation in the VM
    ** to essentially be the same since it is built from
    ** purely static input data. To get benefit from
    ** collapsing we need to add another level or be able
    ** to operate on dynamic inputs from within the VM
    */
    def factorialTest() = {
        // Interpret factorial
        check(runVM(vm_src, fac_src))("Cst(3628800)")

        // Compile factorial
        // check(runVM(vmc_src, fac_src))("Code(Lit(3628800))")

        // Stage VM
        println(ev(s"($vm_src '(LDC 10 LDC 20 ADD SEL (LDC 1 JOIN) (LDC -1 JOIN) STOP))"))
        println(ev(s"($vmc_src '(LDC 10 LDC 20 ADD SEL (LDC 1 JOIN) (LDC -1 JOIN) STOP))"))
        println(ev(s"(run 0 ($vmc_src '(LDC 10 LDC 20 ADD SEL (LDC 1 JOIN) (LDC -1 JOIN) STOP)))"))
        println(ev(s"(run 0 (run 0 ($vmc_src '(LDC 10 LDC 20 ADD SEL (LDC 1 JOIN) (LDC -1 JOIN) STOP))))"))

        println(reifyc(runVM(vmc_src, "LDC 10 LDC 20 ADD SEL (LDC 1 JOIN) (LDC -1 JOIN) STOP")))
        println(inject(Run(Lit(0), reifyc(runVM(vmc_src, "LDC 10 LDC 20 ADD SEL (LDC 1 JOIN) (LDC -1 JOIN) STOP")))))
        
        println(reifyc(runVM(vmc_src, """LDC 10 LDC 20 ADD SEL
                                                    (LDC 10 JOIN)
                                                    (LDC -1 JOIN) STOP""")))
        println(inject(Run(Lit(0), reifyc(runVM(vmc_src, """LDC 10 LDC 20 ADD SEL
                                                                (LDC 10 JOIN)
                                                                (LDC -1 JOIN) STOP""")))))

        println(inject(Run(Lit(0), reifyc(runVM(vmc_src, """LDC 10 LDC 20 ADD SEL
                                                                (LDF (RTN) JOIN)
                                                                (LDC -1 JOIN) STOP""")))))

        println(reifyc(ev(s"($vmc_src '(LDC 2 LDC 1 STOP))")))
        println(ev(s"(run 0 ($vmc_src '(LDC 2 LDC 1 STOP)))"))
        println(reifyc(ev(s"($vmc_src '(LDC 2 LDC 1 ADD STOP))")))
        println(ev(s"(run 0 ($vmc_src '(LDC 2 LDC 1 ADD STOP)))"))
        println(reifyc(ev(s"($vmc_src '(LDC 2 LDC 1 CONS STOP))"))) // TODO: fix CONS instr for cells
        println(ev(s"(run 0 ($vmc_src '(LDC 2 LDC 1 CONS STOP)))"))
        
        println(ev(s"""(run 0 ($vmc_src '(
                    NIL LDC 1 CONS LDC 2 CONS LDF  
                      (NIL LDC 3 CONS LDC 4 CONS 
                        LDF 
                          (NIL LDC 5 CONS LDC 6 CONS 
                          LDF (LD (3 1) LD (2 2) LD (1 1) SUB ADD RTN) AP RTN)
                        AP 
                        RTN) 
                      AP STOP)))"""))
        
        // TODO: stage with respect to factorial + benchmarks. Needs solution to branches for code values i.e. SEL when input is Code()
        // TODO: verify machine in Scheme
        // println(ev(s"""(run 0 ($vmc_src '($fac_src)))"""))
    }

  def test() = {

    println("// ------- EVM.test --------")
    factorialTest()

    testDone()
  }
}