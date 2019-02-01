import EBase._
import TestHelpers._
import ELisp._

object EMatcher {
    // Matcher rewritten for EBase
    def matcherTest() = {
        val src = """
                (letrec ((star_loop (lambda (m c) (maybe-lift (letrec ((inner_loop (lambda (s)
                                                                                                (if (eq?  (maybe-lift 'yes) (m s)) (maybe-lift 'yes)
                                                                                                (if (eq?  (maybe-lift 'done) (car s)) (maybe-lift 'no)
                                                                                                (if (eq?  '_ c) (inner_loop (cdr s))
                                                                                                (if (eq?  (maybe-lift c) (car s)) (inner_loop (cdr s))
                                                                                                    (maybe-lift 'no)))))
                                                                            )
                                                                        ))
                                                                        inner_loop
                                                                )
                                                    )
                                    )
                        )
                        (match_here (lambda (r s)
                                                (if (eq?  'done (car r))
                                                    (maybe-lift 'yes)
                                                    (let m (lambda (s)
                                                                (if (eq?  '_ (car r))
                                                                    (if (eq?  (maybe-lift 'done) (car s))
                                                                        (maybe-lift 'no)
                                                                        (match_here (cdr r) (cdr s)))
                                                                (if (eq?  (maybe-lift 'done) (car s))
                                                                    (maybe-lift 'no)
                                                                (if (eq?  (maybe-lift (car r)) (car s))
                                                                    (match_here (cdr r) (cdr s))
                                                                    (maybe-lift 'no)))))
                                                        (if (eq?  'done (car (cdr r))) (m s)
                                                        (if (eq?  '* (car (cdr r))) (((star_loop (match_here (cdr (cdr r)))) (car r)) s)
                                                            (m s)))))
                                        )
                        ))

                        (let match (lambda (r)
                                        (if (eq?  'done (car r))
                                            (maybe-lift (lambda (s) (maybe-lift 'yes)))
                                            (maybe-lift (match_here r)))
                                    )
                            match)
                    )
        """

        val evl = s"(let maybe-lift (lambda (e) e) $src)"
        val cmp = s"(let maybe-lift (lambda (e) (lift e)) $src)"
        println(ev(s"($cmp '(_ * a _ * done))"))

        import java.io._
        val out_file = new FileOutputStream(new File("trace.log"))
        scala.Console.withOut(out_file) {
        try{
            println(ev(s"(run 0 ($cmp '(_ * a _ * done)))"))
        } catch { case e: Throwable => }
        }
        println
        println(Lisp.ev(s"(${Matcher.matcherc_src} '(_ * a _ * done))"))
        println
        println(Lisp.ev(s"(run 0 (${Matcher.matcherc_src} '(_ * a _ * done)))"))
    }

    def test() = {
        matcherTest()
    }
}