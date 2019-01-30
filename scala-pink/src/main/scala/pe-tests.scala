import PE._
import ELisp._
import EPink._
import EBase._

object PETests {
    import TestHelpers._

    def basicTests() = {
        check(ev(s"(run 0 (($cmp '(LDC 10 LDC 15 ADD WRITEC)) '()))"))("Cst(25)")
        check(reifyc(ev(s"(($cmp '(LDC 10 LDC 15 ADD WRITEC)) '())")))("Let(Var(x13),Lam(List(Var(e)),Let(Var(x31),Plus(Lit(15),Lit(10)),Var(x31))),Let(Var(x14),App(Var(x13),List(Sym(.))),Var(x14)))")
        check(ev(s"(run 0 (($cmp '(LDC 10 LDC 15 LD (1 1) ADD WRITEC)) '((5 6 7 8 9))))"))("Cst(20)")
        check(reifyc(ev(s"($cmp '(LDC 10 LDC 15 LD (1 1) ADD WRITEC))")))("Let(Var(x13),Lam(List(Var(e)),Let(Var(x37),Fst_(Var(e)),Let(Var(x40),Fst_(Var(x37)),Let(Var(x48),Plus(Var(x40),Lit(15)),Var(x48))))),Var(x13))")
        check(reifyc(ev(s"""($cmp '(LDC 135
                                        LDF (LDC 10 LD (1 1) ADD RTN)
                                        AP
                                        STOP))""")))("Let(Var(x13),Lam(List(Var(e)),Let(Var(x64),Plus(Lit(135),Lit(10)),Var(x64))),Var(x13))")
        check(ev(s"""(run 0 (($cmp '(LDC 135
                                        LDF (LDC 10 LD (1 1) ADD RTN)
                                        AP
                                        STOP)) '()))"""))("Cst(145)")
        check(ev(s"""(run 0 (($cmp '(LDC 135
                                        LDF (LDC 10 LD (1 1) ADD RTN)
                                        AP
                                        STOP)) '()))"""))("Cst(145)")

        check(ev(s"(run 0 (($cmp '(LDC 10 LD (1 1) ADD SEL (LDC 1 JOIN) (LDC -1 JOIN) STOP)) '((-10))))"))("Cst(-1)")
        check(ev(s"(run 0 (($cmp '(LDC 10 LD (1 1) ADD SEL (LDC 1 JOIN) (LDC -1 JOIN) STOP)) '((10))))"))("Cst(1)")

        check(reifyc(ev(s"""($cmp
                        '(NIL LDC 6 CONS LDF  
                                        (NIL LDC 5 CONS LDC 3 CONS 
                                            LDF 
                                            (LD (2 1) LD (1 2) LD (1 1) ADD ADD RTN) 
                                            AP 
                                            RTN) 
                                        AP STOP
                        ))""")))("Let(Var(x13),Lam(List(Var(e)),Let(Var(x164),Plus(Lit(3),Lit(5)),Let(Var(x172),Plus(Var(x164),Lit(6)),Var(x172)))),Var(x13))")
        check(ev(s"""(run 0 (($cmp
                        '(NIL LDC 6 CONS LDF  
                                        (NIL LDC 5 CONS LDC 3 CONS 
                                            LDF 
                                            (LD (2 1) LD (1 2) LD (1 1) ADD ADD LDC 14 SUB RTN) 
                                            AP 
                                            RTN) 
                                        AP STOP
                        )) '()))"""))("Cst(0)")

        check(ev(s"""(run 0 (($cmp
                        '(NIL LD (1 1) CONS LDF
                                        (NIL LDC 5 CONS LDC 3 CONS 
                                            LDF 
                                            (LD (2 1) LD (1 2) LD (1 1) ADD ADD LDC 14 SUB RTN) 
                                            AP 
                                            RTN) 
                                        AP STOP
                        )) '((137))))"""))("Cst(-131)")

        check(ev(s"(run 0 (($cmp '(LDC 10 LD (1 1) ADD LD (3 2) LD (1 4) MPY EQ WRITEC)) '((-10 -20 -30 0) () (-50 -60))))"))("Cst(1)")

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

        check(deref(ev(s"""(($evl '(
            DUM  NIL  LDF (
                LDC  5  LD  (1 1)  CAR  EQ SEL
                                                (LDC  5  JOIN )
                                                (LDC  6  LD  (1 1)  CAR  EQ  SEL  (LDC  6  JOIN )  (NIL  LD  (1 1)  CAR  EQ  SEL  (NIL  JOIN )  (NIL  LD  (1 1)  CDR  CONS  LD  (2 1)  AP  JOIN )  JOIN )  JOIN )
                RTN)
                CONS  LDF (
                    NIL NIL LDC  6  CONS  LDC  9  CONS  LDC  8  CONS  LDC  7  CONS  CONS  LD  (1 1)  AP
                RTN)
                RAP STOP
        )) '(()))""")))("Cst(6)")

        check(ev(s"""(run 0 (($cmp '(
            DUM  NIL  LDF (
                LDC  5  LD  (1 1)  CAR  EQ SEL
                                                (LDC  137  JOIN )
                                                (LDC  6  LD  (1 1)  CAR  EQ  SEL  (LDC  6  JOIN )  (NIL  LD  (1 1)  CAR  EQ  SEL  (NIL  JOIN )  (NIL  LD  (1 1)  CDR  CONS  LD  (2 1)  AP  JOIN )  JOIN )  JOIN )
                RTN)
                CONS  LDF (
                    NIL NIL LDC  5  CONS  LDC  4  CONS  LDC  8  CONS  LDC  7  CONS  CONS  LD  (1 1)  AP
                RTN)
                RAP WRITEC
        )) '(())))"""))("Cst(137)")

        check(ev(s"""(run 0 (($cmp '(
           NIL LD (1 1) CONS LDF (
               LDC 10 LDC 20 ADD LD (1 1) SUB RTN
           ) AP WRITEC
        )) '((5))))"""))("Cst(-25)")

    }

    def listAccessTest() = {
        check(deref(ev(s"""(($evl '(
            LDC 1 CONS
            LDC 2 CONS
            LDC 3 CONS
            LDC 4 CONS
            CADDDR
            STOP
        )) '())""")))("Cst(1)")

        check(reifyc(ev(s"""($cmp '(
            LDC 1 CONS
            LD (1 1) CONS
            LDC 3 CONS
            LDC 4 CONS
            CADDR
            STOP
        ))""")))("Let(Var(x13),Lam(List(Var(e)),Let(Var(x37),Fst_(Var(e)),Let(Var(x40),Fst_(Var(x37)),Var(x40)))),Var(x13))")

        check(ev(s"""(($evl '(
            LDC -1 LDC -2 CONS LDC -3 CONS LDF (
                LD (1 1) CADDR RTN
            ) AP
            WRITEC
        )) '())"""))("Cst(-1)")

        check(ev(s"""(($evl '(
            LDC -2 LDC -3 CONS LDC + CONS LDF (
                LDC + LD (1 1) CAR EQ SEL
                    (LD (1 1) CADDR LD (1 1) CADR ADD JOIN)
                    (NIL JOIN)
                RTN
            ) AP
            WRITEC
        )) '(()))"""))("Cst(-5)")

        check(deref(ev(s"""(($evl '(
            DUM NIL LDF (
                LDC plus LD (1 1) CAR EQ SEL
                                    (LD (1 1) CADDR LD (1 1) CADR ADD JOIN)
                                    (NIL JOIN)
                RTN)
                CONS LDF (
                    LDC 2 LDC 3 CONS LDC plus CONS LD (1 1) AP RTN
                ) RAP STOP
        )) '())""")))("Cst(5)")
    }

    def factorialTest() = {
        check(ev(s"""(($evl '(NIL LDC 1 CONS LDC 10 CONS LDF
                        (DUM NIL LDF
                            (LDC 0 LD (1 1) EQ SEL
                                (LD (1 2) JOIN)
                                (NIL LD (1 2) LD (1 1) MPY CONS
                                        LD (3 2) LD (1 1) SUB CONS LD (2 1) AP JOIN)
                            RTN)
                        CONS LDF
                            (NIL LD (2 2) CONS LD (2 1) CONS LD (1 1) AP RTN) RAP
                        RTN) AP WRITEC)) '(()))"""))("Cst(3628800)")

        // check(ev(s"""(($cmp '(NIL LDC 1 CONS LDC 10 CONS LDF
        //                 (DUM NIL LDF
        //                     (LDC 0 LD (1 1) EQ SEL
        //                         (LD (1 2) JOIN)
        //                         (NIL LD (1 2) LD (1 1) MPY CONS
        //                                 LD (3 2) LD (1 1) SUB CONS LD (2 1) AP JOIN)
        //                     RTN)
        //                 CONS LDF
        //                     (NIL LD (2 2) CONS LD (2 1) CONS LD (1 1) AP RTN) RAP
        //                 RTN) AP WRITEC)) '(()))"""))("Cst(3628800)")
    }
}