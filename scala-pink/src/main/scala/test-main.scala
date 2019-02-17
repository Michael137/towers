import Base._
import Lisp._
import VM._

object TestMain {
  def main(args: Array[String]) {
    /* Uncomment to run benchmarks.
       Take some time */
    // Bench.test()

    EBase.test()
    ELisp.test()
    PE.test() // VM staged

    SECD.test() // Pink SECD Machine
    EVMComp.test() // SECD compiler
    VMEval.test() // evaluator on top of SECD

    println("// ###### Full testsuite done ######")
  }
}