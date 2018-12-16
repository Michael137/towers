# Pink in Scala (Collapsing Towers of Interpreters)

## Current main body of work
  - [x] Implement the stack-based VM (see [stack-vm.scm](https://github.com/Michael137/towers/tree/master//scheme-pink/stack-vm.scm))
    - [x] jmp
    - [x] ret
    - [x] branch
  - [X] Be able to run fibonacci/factorial testsuite (see [stack-vm.scm](https://github.com/Michael137/towers/tree/master/scheme-pink/stack-vm.scm))
  - [x] Add the stack-machine as a user-level in Pink
    * __[`stack-vm.scala`](https://github.com/Michael137/towers/tree/master/scala-pink/stack-vm.scala)__
    * Followups: tracing stack, factorial has workaround instructions (i.e. {PUSH,SUB,NEG,DUP}ENV, Persistent AP and REP)
  - [X] Stage stack and compare to non-staged version
  - [X] Stage SECD factorial
  - [ ] Experiment with arbitrary levels of stacks
    * Use the stack machine (or alternative CESK, etc.) to implement eval? (as suggested by @namin)
    * Perhaps MultiLISP machine?
    * New types of stage-polymorphic VMs? E.g. integrate into GraalVM
    * Alternative IR/collapsing methodology? Different normalization process?
    * Mixed code vs. non-code operations? E.g. Plus(Lit(0), Lift(Lit(5))) and support for Tup(Code(Var(0)), Tup(Code(Var(1)), Code(Var(2))))

## Run VM (from sbt console)
* sbt compile
* sbt console
* :load src/main/scala/stack-vm.scala
* VM.test()

or to run full Pink testsuite

* sbt test