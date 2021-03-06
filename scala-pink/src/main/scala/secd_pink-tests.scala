import Lisp._
import Pink._
import Base._
import SECD.cmp
import SECD.evl
import SECD.evg

object SECDTests {
   def testBasic() = {
		println("====> TestBasic")
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
		println("====> TestFactorial")
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
		println("====> TestAckermann")
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
		println("====> TestTryFail")
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
    }

    def basicTests() = {
		println("====> BasicTests")
        check(ev(s"((run 0 ($cmp '(LDC 10 LDC 15 ADD WRITEC))) '())"))("Cst(25)")
        check(reifyc(ev(s"($cmp '(LDC 10 LDC 15 ADD WRITEC))")))("Let(Lam(Lit(25)),Var(0))")
        check(ev(s"((run 0 ($cmp '(LDC 10 LDC 15 LD (1 1) ADD WRITEC))) '((5 6 7 8 9)))"))("Cst(20)")
        check(reifyc(ev(s"($cmp '(LDC 10 LDC 15 LD (1 1) ADD WRITEC))")))("Let(Lam(Let(Fst(Var(1)),Let(Fst(Var(2)),Let(Plus(Var(3),Lit(15)),Var(4))))),Var(0))")
        check(reifyc(ev(s"""($cmp '(NIL LDC 135 CONS
                                        LDF (LDC 10 LD (1 1) ADD RTN)
                                        AP
                                        STOP))""")))("Let(Lam(Let(Cons(Lit(135),Sym(.)),Let(Cons(Var(2),Var(1)),Let(Fst(Var(3)),Let(Fst(Var(4)),Let(Plus(Var(5),Lit(10)),Let(Cons(Var(6),Sym(.)),Var(7)))))))),Var(0))")
        check(ev(s"""((run 0 ($cmp '(NIL LDC 135 CONS
                                        LDF (LDC 10 LD (1 1) ADD RTN)
                                        AP
                                        STOP))) '())"""))("Tup(Cst(145),Str(.))")

        check(ev(s"((run 0 ($cmp '(LDC 10 LD (1 1) ADD SEL (LDC 1 JOIN) (LDC -1 JOIN) STOP))) '((-10)))"))("Tup(Cst(-1),Str(.))")
        check(ev(s"((run 0 ($cmp '(LDC 10 LD (1 1) ADD SEL (LDC 1 JOIN) (LDC -1 JOIN) STOP))) '((10)))"))("Tup(Cst(1),Str(.))")

        check(reifyc(ev(s"""($cmp
                        '(NIL LDC 6 CONS LDF  
                                        (NIL LDC 5 CONS LDC 3 CONS 
                                            LDF 
                                            (LD (2 1) LD (1 2) LD (1 1) ADD ADD RTN) 
                                            AP 
                                            RTN) 
                                        AP STOP
                        ))""")))("""Let(Lam(Let(Cons(Lit(6),Sym(.)),Let(Cons(Var(2),Var(1)),Let(Cons(Lit(5),Sym(.)),Let(Cons(Lit(3),Var(4)),Let(Cons(Var(5),Var(3)),Let(Snd(Var(6)),Let(Fst(Var(7)),Let(Fst(Var(8)),Let(Fst(Var(6)),Let(Snd(Var(10)),Let(Fst(Var(11)),Let(Fst(Var(10)),Let(Plus(Var(13),Var(12)),Let(Plus(Var(14),Var(9)),Let(Cons(Var(15),Sym(.)),Var(16))))))))))))))))),Var(0))""")
        check(ev(s"""((run 0 ($cmp
                        '(NIL LDC 6 CONS LDF  
                                        (NIL LDC 5 CONS LDC 3 CONS 
                                            LDF 
                                            (LD (2 1) LD (1 2) LD (1 1) ADD ADD LDC 14 SUB RTN) 
                                            AP 
                                            RTN) 
                                        AP STOP
                        ))) '())"""))("Tup(Cst(0),Str(.))")

        check(ev(s"""((run 0 ($cmp
                        '(NIL LD (1 1) CONS LDF
                                        (NIL LDC 5 CONS LDC 3 CONS 
                                            LDF 
                                            (LD (2 1) LD (1 2) LD (1 1) ADD ADD LDC 14 SUB RTN) 
                                            AP 
                                            RTN) 
                                        AP STOP
                        ))) '((137)))"""))("Tup(Cst(-131),Str(.))")

        check(ev(s"((run 0 ($cmp '(LDC 10 LD (1 1) ADD LD (3 2) LD (1 4) MPY EQ WRITEC))) '((-10 -20 -30 0) () (-50 -60)))"))("Cst(1)")

        check(ev(s"""(($evl '(LDC 10 LDC -141 ADD WRITEC)) '((5)))"""))("Cst(-131)")
        check(ev(s"""(($evl '(LDC 10 LD (1 1) ADD WRITEC)) '((5)))"""))("Cst(15)")
        check(deref(ev(s"""(($evl
                        '(NIL LDC 6 CONS LDF  
                                        (NIL LDC 5 CONS LDC 3 CONS 
                                            LDF 
                                            (LD (2 1) LD (1 2) LD (1 1) ADD ADD LDC 14 SUB RTN) 
                                            AP 
                                            RTN) 
                                        AP STOP
                        )) '())""")))("Cst(0)")

        check(ev(s"""((run 0 ($cmp '(
           NIL LD (1 1) CONS LDF (
               LDC 10 LDC 20 ADD LD (1 1) SUB RTN
           ) AP WRITEC
        ))) '((5)))"""))("Cst(-25)")

    }

    def listAccessTest() = {
		println("====> ListAccessTests")
        check(deref(ev(s"""(($evl '(
            NIL
            LDC 1 CONS
            LDC 2 CONS
            LDC 3 CONS
            LDC 4 CONS
            CADDDR
            STOP
        )) '())""")))("Cst(1)")

        check(reifyc(ev(s"""($cmp '(
            NIL
            LDC 1 CONS
            LD (1 1) CONS
            LDC 3 CONS
            LDC 4 CONS
            CADDR
            STOP
        ))""")))("Let(Lam(Let(Fst(Var(1)),Let(Fst(Var(2)),Let(Cons(Var(3),Sym(.)),Var(4))))),Var(0))")

        // Note the double CONS
        check(ev(s"""(($evl '(
            NIL NIL LDC -1 CONS LDC -2 CONS LDC -3 CONS CONS LDF (
                LD (1 1) CADDR RTN
            ) AP
            WRITEC
        )) '())"""))("Cst(-1)")
        check(ev(s"""((run 0 ($cmp '(
            NIL NIL LDC -1 CONS LDC -2 CONS LDC -3 CONS CONS LDF (
                LD (1 1) CADDR RTN
            ) AP
            WRITEC
        ))) '())"""))("Cst(-1)")

        check(ev(s"""(($evl '(
            NIL NIL LDC -2 CONS LDC -3 CONS LDC + CONS CONS LDF (
                LDC + LD (1 1) CAR EQ SEL
                    (LD (1 1) CADDR LD (1 1) CADR ADD JOIN)
                    (NIL JOIN)
                RTN
            ) AP
            WRITEC
        )) '(()))"""))("Cst(-5)")
        check(ev(s"""((run 0 ($cmp '(
            NIL NIL LDC -2 CONS LDC -3 CONS LDC + CONS CONS LDF (
                LDC + LD (1 1) CAR EQ SEL
                    (LD (1 1) CADDR LD (1 1) CADR ADD JOIN)
                    (NIL JOIN)
                RTN
            ) AP
            WRITEC
        ))) '(()))"""))("Cst(-5)")

        check(deref(ev(s"""(($evl '(
            DUM NIL LDF (
                LDC plus LD (1 1) CAR EQ SEL
                                    (LD (1 1) CADDR LD (1 1) CADR ADD JOIN)
                                    (NIL JOIN)
                RTN)
                CONS LDF (
                    NIL NIL LDC 2 CONS LDC 3 CONS LDC plus CONS CONS LDR (1 1) AP RTN
                ) RAP STOP
        )) '())""")))("Cst(5)")
        check(deref(ev(s"""((run 0 ($cmp '(
            DUM NIL LDF (
                LDC plus LD (1 1) CAR EQ SEL
                                    (LD (1 1) CADDR LD (1 1) CADR ADD JOIN)
                                    (NIL JOIN)
                RTN)
                CONS LDF (
                    NIL NIL LDC 2 CONS LDC 3 CONS LDC plus CONS CONS LDR (1 1) AP RTN
                ) RAP STOP
        ))) '())""")))("Cst(5)")

        check(ev(s"""((run 0 ($cmp '(
            DUM NIL LDF
                    (LDC plus LD (1 1) CAR EQ SEL
                            (LD (1 1) CADDR LD (1 1) CADR ADD JOIN)
                            (LDC mul LD (1 1) CAR EQ SEL
                                (LD  (1 1) CADDR LD (1 1) CADR MPY JOIN)
                                (LDC minus LD (1 1) CAR EQ SEL
                                    (LD  (1 1) CADDR LD (1 1) CADR SUB JOIN)
                                    (NIL JOIN)
                                JOIN)
                            JOIN)
                        RTN) CONS LDF
                    (NIL NIL LDC 3 CONS LDC 2 CONS LDC mul CONS CONS LDR (1 1) AP RTN) RAP STOP
        ))) '())"""))("Tup(Cst(6),Str(.))")
    }
}
