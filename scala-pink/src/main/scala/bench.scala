import org.scalameter._

import Base._
import Lisp._
import PinkBase._
import Pink._
import VM._

object Bench {
  def bench(thunk: => Unit) = {    
    val time = measure {
      for (i <- 0 until 500 /*100000*/) {
        thunk
      }
    }
    time
  }

  def test() = {
    println("// ------- Bench.test --------")
    println("fac #,evaluated,compiled,traced evaluated,traced compiled,vm-evaluated,vm-compiled")
    val oldLog = Base.log
    try {
      var counter = 0
      Base.log = {x => counter += 1 }
      val Code(fac_compiled) = ev(s"($evalc_src (quote $fac_src))")
      val Code(fac_traced_compiled) = ev(s"($trace_n_evalc_src (quote $fac_src))")
      val trace_n_eval_src = ev_nil(ev_nolift(evt_poly_src))
      val trace_n_eval_val = parseExp(trace_n_eval_src)
      val trace_n_eval_exp1 = trans(trace_n_eval_val, List("arg1"))

      def vm_fac_src(n: Int) = s"""
                          (let eval      $eval_src
                              (let vm_src    '$vmc_src
                                (let fac_src    '(${getFacSource(n)})
                                  ((eval vm_src) fac_src))))"""
      def tripleFacInterp(n: Int) = s"""
        (let eval0 $eval_src
            (let eval1 (quote $eval_src)
                (let eval2 (quote $eval_src)
                    (let vm_src   '$vmc_src
                        (let fac_src    '(${getFacSource(n)})
                            ((((eval0 eval1) eval2) vm_src) fac_src))))))
      """
      
      // val Code(vm_fac_compiled) = ev(vm_fac_src(10))
      // val Code(triple_vm_fac_compiled) = ev(tripleFacInterp(10))
      // println(vm_fac_compiled)
      // println(fac_traced_compiled)

      for (i <- 0 until 4) {
        val t1 = bench(run { evalms(List(fac_val),App(App(eval_exp1,Var(0)),Lit(i))) })
        val t2 = bench(run { evalms(Nil,App(fac_compiled,Lit(i))) })
        val t3 = bench(run { evalms(List(fac_val),App(App(trace_n_eval_exp1,Var(0)),Lit(i))) })
        val t4 = bench(run { evalms(Nil,App(fac_traced_compiled,Lit(i))) })

        val src = parseExp(vm_src)
        val fac = trans(parseExp(s"'(${getFacSource(i)})"), Nil)
        val t5 =  bench(run { evalms(List(src),App(App(eval_exp1,Var(0)),fac)) })

        val Code(code6) = ev(vm_fac_src(i))
        val t6 = bench(run { evalms(Nil,code6) })

        println(code6)
        val Code(code7) = ev(tripleFacInterp(i))
        println(code7)
        val t7 = bench(run { evalms(Nil,code7) })

        println(s"$i,$t1,$t2,$t3,$t4,$t5,$t6,$t7")
      }
    } finally {
      Base.log = oldLog
    }
  }
}
