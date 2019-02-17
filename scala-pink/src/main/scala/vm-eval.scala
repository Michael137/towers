import EVMComp._
import TestHelpers._

object VMEval {
    def test() = {
        println("// ------- VMEVal.test --------")
        evalOnVM("""(letrec (eval) ((lambda (ops)
                                            (if (atom? ops)
                                                ops
                                                (if (eq? (car ops) 'plus)
                                                    (+ (eval (cadr ops)) (eval (caddr ops)))
                                                    (eval (cdr ops))))))
                                            (eval (list plus 2 (plus 2 2))))""", "'()")

        testDone()
    }
}