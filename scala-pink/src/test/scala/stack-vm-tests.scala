import VM._
import org.scalatest.FunSuite

class StackVmTests extends FunSuite {
    test("Run VM test suite") {
        Base.test()
        Lisp.test()
        // Pink.test()
        // Pink_CPS.test()
        // Pink_clambda.test()
        Matcher.test()
        VM.test()
        // Bench.test()
    }
}