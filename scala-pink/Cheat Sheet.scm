; .scm extension to check parens in IDE

Convert to Pink to base-lang: trans(parseExp(<pink source string>), Nil)
Evaluate base-lang: evalms(Nil, <base-lang IR>)
Imports: import Base._, Pink._, Lisp._, VM._
Loading file in sbt:
    1. sbt
    2. console
    3. :load stack-vm.scala
    (3a. import VM._)
    4. VM.test()
Sym(.): indicates empty list/nil
To execute code in Pink:
    1. evalms(Nil,App(Lam(Lift(trans(parseExp(vm_src), Nil))), Sym("ABC")))
    (2. if match error occurs. Try running command again before debugging)
    (3. if reifyc(evalms ...) throws error try evalms(...) separately and then retry reifyc)

Example non-terminating snippet:
    val src = "(let machine (lambda machine s (lambda _ e (lambda _ c (lambda _ d (lambda _ ops
            (((((machine s) e) c) d) ops)
            )))))
    (let start (lambda start ops
        (((((machine 1) 2) 3) 4) ops)
    )
    start
    ))"
    checkrun(s"($src 1)", "Str(yes)")

Example terminating snippet:
    val src = "(let machine (lambda machine s (lambda _ e (lambda _ c (lambda _ d (lambda _ ops
        (if (eq? (car ops) 2) 'Yes
        (((((machine s) e) c) d) (cdr ops)))
        )))))
    (let start (lambda start ops
    (((((machine 1) 2) 3) 4) ops)
    )
    start
    ))"
    checkrun(s"($src '(1 2))", "Str(Yes)")

Interpretation: println(trans(parseExp(s"(($matcher_src '(_ * a _ * done)) '(b a done))"),Nil))
Compilation: println(trans(parseExp(s"((run 0 ($matcherc_src '(_ * a _ * done))) '(b a done))"), Nil))

val instr_poly_src = Pink.ev_poly_src.replace("(env exp)", "(if (eq? 's exp) (log (maybe-lift2 0) (env exp)) (env exp))")
val instr_src = ev_nil(ev_nolift(s"(let maybe-lift2 (lambda _ x x) $instr_poly_src)"))
val instr2_src = ev_nil(ev_nolift(s"(let maybe-lift2 (lambda _ x (lift x)) $instr_poly_src)"))
val instrc_src = ev_nil(ev_lift(s"(let maybe-lift2 (lambda _ x (lift (lift x))) $instr_poly_src)"))
ev(s"((($instr_src '$matcher_src) '(a b done)) '(a b done))")

pink.scala examples: testCorrectnessOptimality()


Compiles:
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
                                (if (eq? 'STOP (car ops)) (maybe-lift s)
                                (if (eq? 'DBG (car ops))
                                  (maybe-lift (car ops))
                                  
                                (((((machine s) e) c) d) (cdr ops))))
                                )))))
              (let start (lambda start ops
                            (((((machine vm-stack) env-list) op-list) call-stack) ops)
                          )
                          start
              )))
      ))))

Let-insertion example:
    reflect(reifyc(evalms(Nil,If(Lift(Lit(0)),Lift(Sym("hi")),Lift(Sym("universe"))))))
    reflect(reifyc(evalms(Nil,If(Lift(Lit(0)),Lift(Sym("hello")),Lift(Sym("world"))))))
    val Code(e) = evalms(Nil,If(Lift(Lit(0)),Lift(Sym("good")),Lift(Sym("bye"))))
    (stBlock foldRight e)(Let)


    println(ev(s"""($vm_src '(
        NIL LDC 1 CONS LDC 3 CONS LDF
          (DUM NIL LDF
            (LDC 0 LD (1 1) EQ SEL
              (LDC 1 JOIN)
              (NIL LD (1 2) LD (1 1) MPY CONS
                LD (3 2) LD (1 1) SUB CONS LD (2 1) AP JOIN)
            RTN)
  
            CONS LDF
              (NIL LD (2 2) CONS LD (2 1) CONS LD (1 1) AP RTN)
              RAP RTN)
            AP DBG)
                              )"""))


                              (if (eq? (car s) (maybe-lift 0))
                                      (((((machine (cdr s)) e) c) (cons (cdddr ops) d)) (caddr ops))
                                      (((((machine (cdr s)) e) c) (cons (cdddr ops) d)) (cadr ops)))

                                      (((((machine '()) (cons (cddr (car s)) (cadr s))) c) (cons (cddr s) (cons (cdr e) (cons (cdr ops) d)))) (caar s))

RAP factorial:
println(ev(s"""($vm_src '(
    NIL LDC 1 CONS LDC 3 CONS LDF
      (DUM NIL LDF
        (LDC 0 LD (1 1) EQ SEL
          (LDC 1 JOIN)
          (NIL LD (1 2) LD (1 1) MPY CONS
              LD (3 2) LD (1 1) SUB CONS DBG LD (2 1) AP JOIN)
        RTN)
      CONS LDF
        (NIL LD (2 2) CONS LD (2 1) CONS LD (1 1) AP RTN) RAP
      RTN) AP DBG
                          ))"""))