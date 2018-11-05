// Regular expression matchers as user-level mini-interpreters

import Base._
import Lisp._

/*
 * Components:
 *
 * * EIP (instruction pointer)
 * * ALU
 * * RAM
 * * Stack
 * * Call stack
 * * Operations:
 */
object Stack_Machine {
  val stack_machine_src = """
  (let star_loop (lambda star_loop m 
    (lambda _ c (maybe-lift (lambda inner_loop s
      (if (eq?  (maybe-lift 'yes) (m s)) (maybe-lift 'yes)
      (if (eq?  (maybe-lift 'done) (car s)) (maybe-lift 'no)
      (if (eq?  '_ c) (inner_loop (cdr s))
      (if (eq?  (maybe-lift c) (car s)) (inner_loop (cdr s)) (maybe-lift 'no)))))))))
    (let match_here (lambda match_here r
      (lambda _ s (if (eq?  'done (car r)) (maybe-lift 'yes)
          (let m (lambda _ s
              (if (eq?  '_ (car r))
                (if (eq?  (maybe-lift 'done) (car s))
                  (maybe-lift 'no)
                  ((match_here (cdr r)) (cdr s)))
                (if (eq?  (maybe-lift 'done) (car s))
                  (maybe-lift 'no)
                  (if (eq?  (maybe-lift (car r)) (car s))
                    ((match_here (cdr r)) (cdr s))
                    (maybe-lift 'no)))))
              (if (eq?  'done (car (cdr r)))
                (m s)
                (if (eq?  '* (car (cdr r)))
                  (((star_loop (match_here (cdr (cdr r)))) (car r)) s)
                  (m s)))))))
        (let match (lambda match r
          (if (eq?  'done (car r))
            (maybe-lift (lambda _ s (maybe-lift 'yes)))
            (maybe-lift (match_here r))))
          match)))
  """

  def test() = {
    println("// ------- Stack_Machine.test --------")
    testDone()
  }
}
