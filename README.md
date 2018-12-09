Collapsing heterogeneous towers
===============================
Current progress
----------------
* __[`stack-vm.scala`](https://github.com/Michael137/towers/tree/master/scala-pink/stack-vm.scala)__: contains stack machine to be evaluated in Pink
  * Instructions on running it are in the __[`README.md`](scala-pink/README.md)__
* graal-towers: use GraalVM as the backend for the base evaluator
  * Enables calling into other languages from Scala (very simple examples in test-main.scala)
  * requires Graal to be installed
  * Instructions on running this version is in ``Makefile''

Next steps
----------
* Extend stack machine
* Compile vs. Eval stack machine and compare results
* Emulate other types of towers
