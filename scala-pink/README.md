# Pink in Scala (Collapsing Towers of Interpreters)

## Current main body of work:results 
  - [x] Implement the stack-based VM (see __[`stack-vm.scm`](stack-vm.scm)__)
    - [x] jmp
    - [x] ret
    - [x] branch
  - [X] Be able to run fibonacci/factorial testsuite (see __[`stack-vm.scm`](stack-vm.scm)__)
  - [ ] Add the stack-machine as a user-level in Pink
    * IN PROGRESS: __[`stack-vm.scm`](stack-vm.scala)__
  - [ ] Stage stack and compare to non-staged version
  - [ ] Experiment with arbitrary levels of stacks
    * Use CESK machine to implement eval? (as suggested by @namin)

## Run VM (from sbt console)
* sbt compile
* sbt console
* :load stack-vm.scala
* VM.test()