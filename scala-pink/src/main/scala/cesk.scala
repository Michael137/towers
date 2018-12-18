// User-level VM
// Summary: CESK machine
// Scheme implementation in scheme-pink/cesk.scm

import Base._
import Lisp._
import PinkBase._
import Pink._

/*
 * C: expression
 * E: first-class function
 * S: first-class function
 * K: list of frames
 * 
 * Supports:
 *    * Branches
 *    * Lambdas
 *    * Let
 *    * Recursive Let
 *    * set!
 *    * Top-level defines
 *    * Call/cc (to model exceptions)
 */
object CESK {
  val cesk_poly_src = """
    (let vm-stack '()
        (let env-list '()
          (let op-list '()
            (let call-stack '()
              (let machine (lambda machine s (lambda _ e (lambda _ c (lambda _ d (lambda _ ops
                                (if (eq? 'STOP (car ops)) s
                                (((((machine s) e) c) d) (cdr ops)))
                                )))))
              (let start (lambda start ops
                            (((((machine vm-stack) env-list) op-list) call-stack) ops)
                          )
                          start
              ))
        ))))
    """

  val cesk_src = s"(let maybe-lift (lambda _  e e) $cesk_poly_src)"
  val ceskc_src = s"(let maybe-lift (lambda _  e (lift e)) $cesk_poly_src)"

  def testInstructions() = {
    // ((λx.λyz.(y-z)+x) 6) 3 5) = 3 - 5 + 6
    ev(s"($cesk_src '(STOP STOP))")
  }

  def test() = {
    println("// ------- CESK.test --------")
    testInstructions()

    testDone()
  }
}

import CESK._