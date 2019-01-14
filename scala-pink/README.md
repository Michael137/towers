# Pink in Scala (Collapsing Towers of Interpreters)

## Current main body of work
  - [x] Implement the stack-based VM (see [stack-vm.scm](https://github.com/Michael137/towers/tree/master/archive/scheme-pink/stack-vm.scm))
    - [x] jmp
    - [x] ret
    - [x] branch
  - [X] Be able to run fibonacci/factorial testsuite (see [stack-vm.scm](https://github.com/Michael137/towers/tree/master/archive/scheme-pink/stack-vm.scm))
  - [x] Add the stack-machine as a user-level in Pink
    * __[`stack-vm.scala`](https://github.com/Michael137/towers/tree/master/scala-pink/src/main/scala/stack-vm.scala)__
    * Followups: tracing stack, factorial has workaround instructions (i.e. {PUSH,SUB,NEG,DUP}ENV, Persistent AP and REP)
  - [X] Stage stack and compare to non-staged version
  - [X] Stage SECD factorial
  - [ ] Experiment with arbitrary levels of stacks
    * Use the stack machine (or alternative CESK, etc.) to implement eval? (as suggested by @namin)
    * Perhaps MultiLISP machine?
    * New types of stage-polymorphic VMs? E.g. integrate into GraalVM
    * Alternative IR/collapsing methodology? Different normalization process?
    * Mixed code vs. non-code operations? E.g. Plus(Lit(0), Lift(Lit(5))) and support for Tup(Code(Var(0)), Tup(Code(Var(1)), Code(Var(2))))
    - [x] Extend λ↑↓ to support mutation of store and env
    - [x] Modify Lisp front-end to match new λ↑↓*
    - [x] Port SECD machine to new tower
    - [ ] Be able to stage through side-effects e.g. pointers and mutation
    - [x] Multi-argument lambdas in e-pink
    - [ ] Letrec in e-pink
    - [ ] Construct following towers:
      - [ ] Base<->VM<->Evaluator<->User program
      - [ ] Base<->Pink<->VM<->Evaluator<->User program
    - [ ] Experiment with more severe side-effects
    - [ ] Formal description

## Run VM (from sbt console)
* sbt compile
* sbt console
* :load src/main/scala/stack-vm.scala
* VM.test()

or to run full Pink testsuite

* sbt test

## Run Test Suite
* sbt ~run

# Debugging
## Intellij
	* Configure remote debugging configuration (should be same as sbt startup port)
	* start builtin sbt shell
	* set breakpoints
	* Run remote debugging configuration
	* Run sbt test
