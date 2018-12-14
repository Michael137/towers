# Issue Log
## evalms Stack overflow
This refers to a stack overflow when staging the stack machine in [stack-vm.scala](stack-vm.scala) w.r.t. a set of instructions that repeatedly call a function based on some break condition.

Example (from testCrash() in [stack-vm.scala](stack-vm.scala)):

These SECD instructions decrement from 10 to 1 while the counter (the top of the stack) is > 1.
```
val src_so = s"""
            NIL LDC 10 CONS
            LDF (
              DUPENV
              LD (1 1)
              PUSHENV 1
              SUBENV
              NEGENV
              LD (1 1)
              GT 1
              SEL (JOIN) (WRITEC)
              REP
              RTN
            ) PAP
            """
```

Simply interpreting it (in the sbt console) we get the expected result:
```
scala> ev(s"($vm_src '($src_so))")
res1: Cst(2)
```

However, compiling it causes a stack overflow. Here is a snippet of the trace.error that gets generated when we run ```VM.test()```

```
scala> ev(s"($vmc_src '($src_so))")
        ...<repeats indefinitely>
	at Base$.evalms(base.scala:219)
	at Base$.$anonfun$evalms$6(base.scala:219)
	at Base$.$anonfun$reifyc$1(base.scala:121)
	at Base$.$anonfun$reify$1(base.scala:64)
	at Base$.run(base.scala:55)
	at Base$.reify(base.scala:61)
	at Base$.reifyc(base.scala:119)
	at Base$.evalms(base.scala:219)
```

Having instrumented the code we following additional information:
```
REIFY CALLED
DEBUGGING f arg in run
DEBUGGING calling f in run
DEBUGGING f arg in reify
DEBUGGING reifyc pre-code
DEBUGGING reifyc post-code
DEBUGGING last in reify
DEBUGGING finished calling f in run: Var(2)
REIFY CALLED
DEBUGGING f arg in run
DEBUGGING calling f in run
DEBUGGING f arg in reify
DEBUGGING reifyc pre-code
DEBUGGING if: condition = Equ(Fst(Var(7)),App(Var(0),Lit(0)))
              conseq = App(App(App(App(App(Var(6),Snd(Var(7))),Var(9)),Var(11)),Cons(Snd(Snd(Snd(Var(15)))),Var(13))),Fst(Snd(Snd(Var(15)))))
              alt = App(App(App(App(App(Var(6),Snd(Var(7))),Var(9)),Var(11)),Cons(Snd(Snd(Snd(Var(15)))),Var(13))),Fst(Snd(Var(15))))
```
Here we see that we succesfully evaluate the condition and then call ```reifyc(evalms(env, <consequence>))```. This then never leaves ```reifyc()``` (indicated by *DEBUGGING reifyc pre-code*).

Comparing these base-lang tags to the Pink implementation of the stack-machine we can pinpoint the repeating code to:
```
(if (eq? 'SEL (car ops))
    (if (eq? (car s) (maybe-lift 0))
        (((((machine (cdr s)) e) c) (cons (cdddr ops) d)) (caddr ops))
        (((((machine (cdr s)) e) c) (cons (cdddr ops) d)) (cadr ops)))
```

Interestingely when replacing the last argument (i.e. the next instructions to execute) to (machine ...) with a quoted list of instructions such as ```'(ADD (STOP ()))```, compilation succeeds.


## evalms Match error