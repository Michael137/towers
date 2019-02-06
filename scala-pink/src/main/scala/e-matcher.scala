import EBase._
import TestHelpers._
import ELisp._

object EMatcher {
    // Matcher rewritten for EBase
    def matcherTest() = {
        val src = """
                (letrec ((star_loop (lambda (matcher) (lambda (char) (maybe-lift (letrec ((inner_loop (lambda (string)
                                                                                                (if (eq?  (maybe-lift 'yes) (matcher string)) (maybe-lift 'yes)
                                                                                                (if (eq?  (maybe-lift 'done) (car string)) (maybe-lift 'no)
                                                                                                (if (eq?  '_ char) (inner_loop (cdr string))
                                                                                                (if (eq?  (maybe-lift char) (car string)) (inner_loop (cdr string))
                                                                                                    (maybe-lift 'no)))))
                                                                            )
                                                                        ))
                                                                        inner_loop
                                                                )
                                                    )
                                                )
                                    )
                        )
                        (match_here (lambda (r) (lambda (s)
                                                (if (eq?  'done (car r))
                                                    (maybe-lift 'yes)
                                                    (let m (lambda (str)
                                                                (if (eq?  '_ (car r))
                                                                    (if (eq?  (maybe-lift 'done) (car str))
                                                                        (maybe-lift 'no)
                                                                        (match_here (cdr r) (cdr str)))
                                                                (if (eq?  (maybe-lift 'done) (car str))
                                                                    (maybe-lift 'no)
                                                                (if (eq?  (maybe-lift (car r)) (car str))
                                                                    (match_here (cdr r) (cdr str))
                                                                    (maybe-lift 'no)))))
                                                        (if (eq? 'done (car (cdr r)))
                                                            (m s)
                                                        (if (eq? '* (car (cdr r)))
                                                            (((star_loop (match_here (cdr (cdr r)))) (car r)) s)
                                                            (m s)))))
                                        ))
                        ))

                        (let match (lambda (reg)
                                        (if (eq? 'done (car reg))
                                            (maybe-lift (lambda (s) (maybe-lift 'yes)))
                                            (maybe-lift (match_here reg)))
                                    )
                            match)
                    )
        """

        val evl = s"(let maybe-lift (lambda (e) e) $src)"
        val cmp = s"(let maybe-lift (lambda (e) (lift e)) $src)"
        println(ev(s"(($evl '(_ a _ * done)) '(b a done))"))
        // println(Lisp.ev(s"((${Matcher.matcher_src} '(_ * a _ * done)) '(b a done))"))

        // import java.io._
        // val out_file = new FileOutputStream(new File("trace.log"))
        // scala.Console.withOut(out_file) {
        // try{
        //     println(ev(s"(run 0 ($cmp '(_ * a _ * done)))"))
        // } catch { case e: Throwable => }
        // }
        println
        println(Lisp.ev(s"(${Matcher.matcherc_src} '(_ * a _ * done))"))
        println
        println(Lisp.ev(s"(run 0 (${Matcher.matcherc_src} '(_ * a _ * done)))"))
    }

    def test() = {
        matcherTest()
    }
}