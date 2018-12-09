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
Sym(.): usually indicate ignored lambda self-references i.e. the "_" in "(lambda _ x ...)"
To execute code in Pink:
    1. evalms(Nil,App(Lam(Lift(trans(parseExp(vm_src), Nil))), Sym("ABC")))
    (2. if match error occurs. Try running command again before debugging)
    (3. if reifyc(evalms ...) throws error try evalms(...) separately and then retry reifyc)