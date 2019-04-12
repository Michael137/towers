import Base._
import Lisp._
import VM._

object TestMain {
  def main(args: Array[String]) {
    /* Uncomment to run Pink benchmarks.
       Take some time */
    // Bench.test()

    EBase.test()
    ELisp.test()
    SECD.test() // Pink SECD Machine
    EVMComp.test() // SECD compiler
    VMEval.test() // evaluator on top of SECD
    VMMatcher.test()

    /* Benchmarks */
    // SECDBench.test()

    /* Uncomment to run benchmarks of
       original framework */
    // Base.test()
    // Lisp.test()
    // Pink.test()

    println("// ###### Full testsuite done ######")
  }
}
