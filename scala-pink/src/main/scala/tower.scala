import Base._
import Lisp._
import PinkBase._
import Pink._
import SECD._
import EVMComp._
import VMEval._

object Tower {
  def main(args: Array[String]) {
    val i = 6
    def meta_eval_fac_src_staged(n: Int) = meta_eval(s"""
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

          ) $n)""", "lift")
    EVMComp.hasLDR = true
    val meta_eval_instrSrc_staged = instrsToString(compile(Lisp.parseExp(meta_eval_fac_src_staged(i)), Nil, Base.Tup(Base.Str("WRITEC"), Base.Str("."))))
    val Code(meta_fac_compiled_staged) = ev(s"(($evg '($meta_eval_instrSrc_staged)) (lift '()))")
    println(prettycode(reifyc(Code(meta_fac_compiled_staged))))
  }
}
