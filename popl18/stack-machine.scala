// Regular expression matchers as user-level mini-interpreters

import Base._
import Lisp._
import PinkBase._
import Pink._

/*
 * Components:
 *
 * * EIP (instruction pointer)
 * * ALU
 * * RAM
 * * Stack
 * * Call stack
 * * Operations:
; Naive stack-machine:
;   (mk-stack) ; ==> ((stack), #t)
;   (push 10) ; ==> ((stack 10), #t)
;   (push 20) ; ==> ((stack 20), #t)
;   (add) ; ==> ((stack 30), #t)
;   (pop) ; ==> ((stack), 30)
;   (pop) ; ==> '(Stack underflow)

(define global-stack '())
(define (mk-stack)
    (begin
        (set! global-stack (list 'stack))
        global-stack))
(define (push num)
    (begin
        (set! global-stack (append global-stack (list num)))
        (cons global-stack #t)))

(define (pop)
    (if (eq? (stack-empty? global-stack) #f)
        (begin
            (set! ret (car (cdr global-stack)))
            (set! global-stack (append (list 'stack) (cdr (cdr global-stack))))
            (cons global-stack ret))
        (display '(Error: stack underflow))))
    

(define (stack-empty? stk)
    (<= (length stk) 1))

Pink impl:
  (let stack
    (let stack_ '(stack)
      (let exec (lambda exec op (
                              (if (eq?  (maybe-lift 'PUSH) (car op)) (maybe-lift (cons stack_ (cadr op))) (maybe-lift stack_))
                            )
                )
          exec
      )
    )
  )
 */
 
object Stack_Machine {
  val sm_poly_src = """
  (let star_loop (lambda star_loop m (lambda _ c (maybe-lift 'done)))
      (let match_here (lambda match_here r (lambda _ s (maybe-lift 'done)))
        (let stack (lambda stack r
                        (if (eq?  'done (car r)) (maybe-lift (lambda _ s (maybe-lift 'yes))) (maybe-lift 'no))
                    )
            stack
        )
      )
  )
    """

  // Example: ev(s"((($eval_src '$sm_src) '(done)) '(a done))")

  val sm_src = s"(let maybe-lift (lambda _  e e) $sm_poly_src)"
  val smc_src = s"(let maybe-lift (lambda _  e (lift e)) $sm_poly_src)"

/*
  val sm_val = parseExp( sm_src )
  val sm_expr = trans(sm_val, Nil)  
*/

  def test() = {
    println("// ------- Stack_Machine.test --------")

    checkrun(s"(($sm_src '(_ * a _ * done)) '(b a done))", "Str(yes)")
    checkrun(s"(($sm_src '(_ * a _ * done)) '(b b done))", "Str(no)")
    ev(s"((($eval_src '$sm_src) '(a b done)) '(a b done))")

    testDone()
  }
}