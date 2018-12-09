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