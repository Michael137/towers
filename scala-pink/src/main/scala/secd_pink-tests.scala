import Lisp._
import Pink._
import Base._
import SECD.cmp
import SECD.evl
import SECD.evg

object SECDTests {
   def testBasic() = {
        val prog1 = "'(LDC 10 LDC 15 ADD WRITEC)"
        val prog2 = "'(LDC 10 LDC 15 LD (1 1) ADD WRITEC)"
        val arg2 = "'((5 6 7 8 9))"


        println(reifyc(ev(s"(($cmp $prog1) (lift '()))")))
        check(ev(s"((run 0 ($cmp $prog1)) '())"))("Cst(25)")

        check(ev(s"(($evl $prog2) $arg2)"))("Cst(20)")

        check(ev(s"((run 0 ($cmp $prog2)) $arg2)"))("Cst(20)")

        println(reifyc(ev(s"""($cmp '(NIL LDC 135 CONS
                                        LDF (LDC 10 LD (1 1) ADD RTN)
                                        AP
                                        STOP))""")))
        check(ev(s"(($evl $prog1) '())"))("Cst(25)")
   }

    def factorialTests() = {
        val factorialProg = """'(NIL LDC 1 CONS LDC 10 CONS LDF
                (DUM NIL LDF
                (LDC 0 LD (1 1) EQ SEL
                (LD (1 2) JOIN)
                (NIL LD (1 2) LD (1 1) MPY CONS
                LD (3 2) LD (1 1) SUB CONS LDR (1 1) AP JOIN)
                RTN)
                CONS LDF
                (NIL LD (2 2) CONS LD (2 1) CONS LDR (1 1) AP RTN) RAP
                RTN) AP WRITEC)"""
        check(ev(s"(($evl $factorialProg) '())"))("Cst(3628800)")

        println(reifyc(ev(s"(($cmp $factorialProg) (lift '()))")))
        check(ev(s"((run 0 ($cmp $factorialProg)) '())"))("Cst(3628800)")

        println(prettycode(reifyc(ev(s"(($cmp $factorialProg) (lift '()))"))))

        // println(prettycode(PinkBase.fac_exp_anf)) // Factorial without SECD for comparison
    }

    def testAckermann() = {
        val ack_prog = """'(
            DUM NIL LDF
                (LDC 0 LD (1 1) EQ SEL
                        (LDC 1 LD (1 2) ADD JOIN ) (LDC 0 LD (1 2) EQ SEL
                (NIL LDC 1 CONS LDC 1 LD (1 1) SUB CONS LDR (1 1) AP JOIN ) (NIL NIL LDC 1 LD (1 2) SUB CONS LD (1 1) CONS LDR (1 1) AP CONS LDC 1 LD (1 1) SUB CONS LDR (1 1) AP JOIN ) JOIN ) RTN ) CONS LDF
                (NIL LD (2 1) CONS LD (2 2) CONS LDR (1 1) AP RTN ) RAP WRITEC
        )"""
        check(ev(s"((run 0 ($cmp $ack_prog)) '((2 2)))"))("Cst(7)")
        println(prettycode(reifyc(ev(s"(($cmp $ack_prog) (lift '()))"))))
    }

    def testTryFail() = {
        val ap0 = """'(
            LDF (LD (1 1) LD (1 2) ADD RTN) AP0 STOP
        )"""
        check(ev(s"(($evl $ap0) '((2 2)))"))("Tup(Cst(4),Str(.))")
        check(ev(s"((run 0 ($cmp $ap0)) '((2 2)))"))("Tup(Cst(4),Str(.))")

        val tryFail = """'(
            LDC 3 LDF (TRY (LDC 4 RTN)
                        TRY (LDC 3 RTN)
                        TRY (LDC 2 RTN)
                        TRY (LDC 1 RTN)
                        TRY (LDC 0 RTN)
                        TRY (LDC -1 RTN)
                        FAIL)
            AP0 GT SEL (FAIL JOIN) (LDC done JOIN) WRITEC
        )"""
        check(ev(s"(($evl $tryFail) '())"))("Str(done)")
        // check(ev(s"($cmp $tryFail)"))("") // TODO: @crash because staging of try/fail is not yet implemented

        val ldtTest = """'(
            NIL LDT
                (TRY_ (LDC 0 RTN)
                TRY_ (LDC 2 RTN)
                TRY_ (LDC 3 RTN) FAIL_ )
                CONS LDF (LDC 2 NIL LD (1 1) AP_ LT SEL
                        (LDC 1 JOIN )
                        (FAIL_ JOIN ) RTN ) AP_ WRITEC
        )"""

        check(ev(s"(($evl $ldtTest) '())"))("Cst(1)")
        // check(ev(s"(($cmp $ldtTest) '())"))("Cst(1)")
    }
}