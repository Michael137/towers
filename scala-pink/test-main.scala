object TestMain {
  def main(args: Array[String]) {
    Base.test()
    Lisp.test()
    //Pink.test()
    //Pink_CPS.test()
    //Pink_clambda.test()
    //Matcher.test()
    Stack_Machine.test()
    VM.test()
    //Bench.test()
    println("DONE")
  }
}