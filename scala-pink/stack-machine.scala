// User-level stack machine

import Base._
import Lisp._
import PinkBase._
import Pink._

object Stack_Machine {
  val sm_poly_src = """
  (let stack '()
    (let exec (lambda exec stk (lambda _ op
                                  (if (eq? (maybe-lift 'PUSH) (car op)) (exec (maybe-lift (cons stk (cadr op))))
                                  (if (eq? (maybe-lift 'POP) (car op)) (exec (maybe-lift (car stk)))
                                  (if (eq? (maybe-lift 'ADD) (car op)) (exec (maybe-lift (cons (cddr stk) (+ (car stk) 
                                                                                                             (car (cdr stk)))
                                                                                  )
                                                                      ))
                                  (if (eq? (maybe-lift 'DONE) (car op)) (maybe-lift stk)

                                  (exec (maybe-lift stk))
                                  ))))
                              )
              )
        (exec stack)
    )
  )
    """

  // Example: ev(s"((($eval_src '$sm_src) '(PUSH 10)) '(POP))")

  val sm_src = s"(let maybe-lift (lambda _  e e) $sm_poly_src)"
  val smc_src = s"(let maybe-lift (lambda _  e (lift e)) $sm_poly_src)"

  // TODO: tracing stack

  def test() = {
    println("// ------- Stack_Machine.test --------")

    ev(s"(((($eval_src '$sm_src) '(PUSH 10)) '(POP)) '(PUSH 30))")
    ev(s"(((((($sm_src '(PUSH 0)) '(PUSH 23)) '(PUSH 42)) '(POP)) '(PUSH 137)) '(DONE))")

    testDone()
  }
}