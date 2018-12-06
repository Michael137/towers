# Pink in Scheme (Collapsing Towers of Interpreters)

## Code
* __[`base.scm`](base.scm)__ defines the multi-level core language λ↑↓ as a definitional interpreter in Scheme.
* __[`pink.scm`](pink.scm)__ defines the meta-circular stage-parametric interpreter for Pink on top of the base.
* __[`matcher.scm`](matcher.scm)__ defines a matcher as an example on top of Pink.
* __[`mk.scm`](mk.scm)__ defines a µKanren as an example on top of Pink.
* __[`stack-vm.scm`](stack-vm.scm)__ is an initial implementation of a stack-machine later to be ported to Pink

## Current main body of work:results 
  - [ ] Implement the stack-based VM (in-progress)
    - [x] jmp
    - [x] ret
    - [ ] branch
  - [ ] Be able to run fibonacci/factorial testsuite
  - [ ] Add the stack-machine as a user-level in Pink (most likely Scala)
  - [ ] Stage stack and compare to non-staged version
  - [ ] Experiment with arbitrary levels of stacks
    * Use CESK machine to implement eval? (as suggested by @namin)

## Run
Each code file `.scm` above has a companion `-tests.scm`, which can be run with [Chez Scheme](https://cisco.github.io/ChezScheme/).
For example, `chez pink-tests.scm` runs all the Pink-related tests.