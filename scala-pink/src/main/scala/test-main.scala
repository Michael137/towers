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
  
    // VM.test()
    // Bench.test()

    // EBase.test()
    // ELisp.test()
    // PE.test()
    EVMComp.test()
    // EPink.test() // TODO
    // EVM.test()
    println("// ###### Full testsuite done ######")
  }
}