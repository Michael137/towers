Collapsing heterogeneous towers
===============================
Current progress
----------------
* stack-machine.scala: contains very simple stack machine to be evaluated in Pink
  * Instructions on running it are in the file
* graal-towers: use GraalVM as the backend for the base evaluator
  * Enables calling into other languages from Scala (very simple examples in test-main.scala)
  * requires Graal to be installed
  * Instructions on running this version is in ``Makefile''

Next steps
----------
* Extend stack machine
* Compile vs. Eval stack machine and compare results
* Emulate other types of towers