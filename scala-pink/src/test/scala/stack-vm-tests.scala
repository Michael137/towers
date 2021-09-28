import VM._
import org.scalatest.flatspec.AnyFlatSpec

class StackVmTests extends AnyFlatSpec  {
    it should "Run VM test suite" in {
        Base.test()
        Lisp.test()
        // Pink.test()
        // Pink_CPS.test()
        // Pink_clambda.test()
        Matcher.test()
        // VM.test() -- TODO: fix
        // Bench.test()
    }
}
