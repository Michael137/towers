import org.scalameter._

import Base._
import Lisp._
import PinkBase._
import Pink._
import SECD._
import EVMComp._
import VMEval._

object SECDBench {
  def bench(thunk: => Unit) = {    
    val time = measure {
      for (i <- 0 until 1000 /*100000*/) {
        thunk
      }
    }
    time
  }

  def test() = {
    println("// ------- Bench.test --------")
    println("fac #,lisp (evaluated),lisp,lisp+SECD,lisp+SECD+MetaEval,lisp+SECD+MetaEval (staged)")
    val oldLog = Base.log
    try {
      var counter = 0
      Base.log = {x => counter += 1 }
      val fac_y_comb = s"""
      ((lambda _ fun
            ((lambda _ F
              (F F))
            (lambda _ F
              (fun (lambda _ x ((F F) x))))))

        (lambda _ factorial
          (lambda _ n
            (if (eq? n 0)
                1
                (* n (factorial (- n 1))))))

        )
      """
      val Code(fac_compiled) = ev(s"($evalc_src (quote $fac_y_comb))") // or $fac_src
      val secd_fac = """'(NIL LDC 1 CONS LD (1 1) CONS LDF
                        (DUM NIL LDF
                        (LDC 0 LD (1 1) EQ SEL
                        (LD (1 2) JOIN)
                        (NIL LD (1 2) LD (1 1) MPY CONS
                        LD (3 2) LD (1 1) SUB CONS LDR (1 1) AP JOIN)
                        RTN)
                        CONS LDF
                        (NIL LD (2 2) CONS LD (2 1) CONS LDR (1 1) AP RTN) RAP
                        RTN) AP WRITEC)"""
      println(prettycode(reifyc(Code(fac_compiled))))
      val fac_eval = parseExp(fac_y_comb) // or use fac_val

      val Code(secd_fac_compiled) = ev(s"($cmp $secd_fac)")

      def meta_eval_fac_src(n: Int) = meta_eval(s"""(((lambda (fun)
                                          ((lambda (F)
                                            (F F))
                                          (lambda (F)
                                            (fun (lambda (x) ((F F) x))))))

                                      (lambda (factorial)
                                        (lambda (n)
                                          (if (eq? n 0)
                                              1
                                              (* n (factorial (- n 1))))))

                                      ) $n)""")
        def meta_eval_fac_src_staged(n: Int) = meta_eval(s"""(((lambda (fun)
              ((lambda (F)
                (F F))
              (lambda (F)
                (fun (lambda (x) ((F F) x))))))

          (lambda (factorial)
            (lambda (n)
              (if (eq? n 0)
                  1
                  (* n (factorial (- n 1))))))

          ) $n)""", "lift")
      for (i <- 0 until 8) {
        val t1 = bench(run { evalms(List(fac_eval),App(App(eval_exp1,Var(0)),Lit(i))) })
        val t2 = bench(run { evalms(Nil,App(fac_compiled,Lit(i))) })

        val t3 = bench(run { evalms(Nil, App(secd_fac_compiled,Cons(Cons(Lit(i), Sym(".")), Sym(".")))) })

        // TODO: don't do this in loop.
        //       Requires some tinkering to pass argument to underlying SECD machine properly
        EVMComp.hasLDR = true
        val meta_eval_instrSrc = instrsToString(compile(ELisp.parseExp(meta_eval_fac_src(i)), Nil, EBase.Tup(EBase.Str("STOP"), EBase.Str("."))))
        val meta_eval_instrSrc_staged = instrsToString(compile(ELisp.parseExp(meta_eval_fac_src_staged(i)), Nil, EBase.Tup(EBase.Str("WRITEC"), EBase.Str("."))))
        val Code(meta_fac_compiled) = ev(s"(($cmp '($meta_eval_instrSrc)) (lift '()))")
        val Code(meta_fac_compiled_staged) = ev(s"(($evg '($meta_eval_instrSrc_staged)) (lift '()))")
        // println(prettycode(reifyc(Code(meta_fac_compiled_staged))))
        // println(prettycode(reifyc(Code(meta_fac_compiled))))

        val t4 = bench(run { evalms(Nil, meta_fac_compiled) })
        val t5 = bench(run { evalms(Nil, meta_fac_compiled_staged) })
        println(s"$i,$t1,$t2,$t3,$t4,$t5")
      }
    } finally {
      Base.log = oldLog
    }
  }
}
