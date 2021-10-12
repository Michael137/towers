import Base._
import Lisp._
import PinkBase._
import Pink._
import SECD._
import EVMComp._
import VMEval._

object Tower {
  def main(args: Array[String]) {
    val slowOK = args.length > 0
    def meta_eval_fac_src_staged = meta_eval(s"""(lambda (n)
         (((lambda (fun)
              ((lambda (F)
                (F F))
              (lambda (F)
                (fun (lambda (x) ((F F) x))))))

          (lambda (factorial)
            (lambda (n)
              (if (eq? n 0)
                  1
                  (* n (factorial (- n 1))))))

          ) n))""", "lift")
    EVMComp.hasLDR = true
    val meta_eval_instrSrc_staged = instrsToString(compile(Lisp.parseExp(meta_eval_fac_src_staged), Nil, Base.Tup(Base.Str("WRITEC"), Base.Str("."))))
    val expc = s"(($evg '($meta_eval_instrSrc_staged)) (lift '()))"
    val Code(meta_fac_compiled_staged0) = ev(expc)
    val Code(meta_fac_compiled_staged1) = ev(s"($eval_src (quote $expc))")
    assert(meta_fac_compiled_staged0 == meta_fac_compiled_staged1)
    if (slowOK) {
      val Code(meta_fac_compiled_staged2a) = ev(s"(($eval_src (quote $eval_src)) (quote $expc))")
      val Code(meta_fac_compiled_staged2b) = ev(s"($eval_src (quote ($eval_src (quote $expc))))")
      assert(meta_fac_compiled_staged1 == meta_fac_compiled_staged2a)
      assert(meta_fac_compiled_staged1 == meta_fac_compiled_staged2b)
    }
    println(prettycode(reifyc(Code(meta_fac_compiled_staged1))))
    println(evalms(Nil,App(meta_fac_compiled_staged1,Lit(6))))
  }
}
