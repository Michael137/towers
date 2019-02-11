import Base._
import Lisp._
import VM._

object TestMain {
  def main(args: Array[String]) {
    // Base.test()
    // Lisp.test()
    // Pink.test()
    // Pink_CPS.test()
    // Pink_clambda.test()
    // Matcher.test()

    // EPink.test()
    // EVM.test()
    // EMatcher.test()
  
    // VM.test()
    // Bench.test()

    /* Main Tests */
    // EBase.test()
    // ELisp.test()
    // PE.test() // VM staged
    // EVMComp.test() // SECD compiler

    SECD.test() // Pink SECD Machine

    println("// ###### Full testsuite done ######")
  }
}