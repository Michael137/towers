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
 */
 
object Stack_Machine {
  val sm_poly_src = """
  (let star_loop (lambda star_loop m (lambda _ c (maybe-lift (lambda inner_loop s
  (if (eq?  (maybe-lift 'yes) (m s)) (maybe-lift 'yes)
  (if (eq?  (maybe-lift 'done) (car s)) (maybe-lift 'no)
  (if (eq?  '_ c) (inner_loop (cdr s))
  (if (eq?  (maybe-lift c) (car s)) (inner_loop (cdr s)) (maybe-lift 'no)))))))))
(let match_here (lambda match_here r (lambda _ s (if (eq?  'done (car r)) (maybe-lift 'yes)
  (let m (lambda _ s
      (if (eq?  '_ (car r)) (if (eq?  (maybe-lift 'done) (car s)) (maybe-lift 'no) ((match_here (cdr r)) (cdr s)))
      (if (eq?  (maybe-lift 'done) (car s)) (maybe-lift 'no)
      (if (eq?  (maybe-lift (car r)) (car s)) ((match_here (cdr r)) (cdr s)) (maybe-lift 'no)))))
    (if (eq?  'done (car (cdr r))) (m s)
    (if (eq?  '* (car (cdr r))) (((star_loop (match_here (cdr (cdr r)))) (car r)) s) (m s)))))))
(let match (lambda match r
  (if (eq?  'done (car r)) (maybe-lift (lambda _ s (maybe-lift 'yes))) (maybe-lift (match_here r))))
match)))"""

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

    testDone()
  }
}