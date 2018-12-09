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