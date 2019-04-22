import Base._
import Lisp._
import VM._

object TestMain {
  def main(args: Array[String]) {
    /* Uncomment to run Pink benchmarks.
       Take some time */
    // Bench.test()

    Base.test()
    Lisp.test()
    Pink.test()

    SECD.test() // Pink SECD Machine
    EVMComp.test() // SECD compiler
    VMEval.test() // evaluator on top of SECD
    VMMatcher.test()
    VMLiftedMatcher.test()

    /* Benchmarks */
    // SECDBench.test()

    println("// ###### Full testsuite done ######")
  }
}
