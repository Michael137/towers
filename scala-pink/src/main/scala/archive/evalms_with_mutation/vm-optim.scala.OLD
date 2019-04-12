import scala.collection.mutable.ListBuffer

object Optimizer {

    /*
    ** iter: Container representing stBlock that contains lifted Val objects
    ** strat: Optimization pass (strategy) to apply to stBlock.
    **        Takes unoptimized container and returns an optimized version
    */
    def optimStBlock[B <: Traversable[_]](iter: B, strat: (B => B)): B = {
        strat(iter)
    }

    /*
    ** iter: Container representing stBlock that contains lifted Val objects
    **
    ** Description: will reduce consecutive Lisp list primitives e.g. car, cdr, cons, etc.
    */
    def listAccessStrat[B <: Traversable[_]](iter: B): B = {
        /* Collect list ops */
        var idx = 0
        var fold = false
        var allFolds: ListBuffer[ListBuffer[(Int, Any)]] = ListBuffer(ListBuffer())
        var currFold: ListBuffer[(Int, Any)] = ListBuffer()
        iter.foreach({ x => x match {
                // TODO: genericize w.r.t. Base object
                case l: Base.ListOp => currFold += ((idx, l))
                case _ => 
                    allFolds += currFold
                    currFold = ListBuffer()
            }

            idx += 1
        })
        allFolds += currFold

        /* Reduce consecutive list ops */
        var foldedExps = ListBuffer[Any]()
        allFolds.foreach({ lst => lst match {
            case l if(l != ListBuffer()) =>
                var acc +: rst = lst
                var folded = rst.foldLeft(acc)({ (acc, e) => (acc, e) match {
                    // TODO: generecize to simply ListOp
                    case ((i, v: Base.Exp), (_, l: Base.Fst)) => (i, Base.Fst(v))
                    case ((i, v: Base.Exp), (_, l: Base.Snd)) => (i, Base.Snd(v))
                }})

                if(rst != ListBuffer())
                    foldedExps += ((folded, rst.last._1)) // (New expressions, last element in original block)
            case _ => ()
        }})

        /* Stitch back stBlock */
        var snippets = ListBuffer[Tuple2[Int, Any]]() // (idx, exp)
        var res = ListBuffer[Any]()
        var curFrom = 0
        foldedExps.foreach({ x =>
            var ((from: Int, v: Base.Exp), to: Int) = x
            if(curFrom != from)
                snippets = snippets :+ (curFrom, iter.asInstanceOf[List[Base.Val]].slice(curFrom, from)) // TODO: genericize
            curFrom = to + 1
        })
        snippets = snippets :+ (curFrom, iter.asInstanceOf[List[Base.Val]].slice(curFrom, iter.size))

        res = res ++ snippets ++ foldedExps.map({x => val (t, _) = x; t})
        res = res.sortWith({(t1, t2) =>
            val ((i: Int, e1), (j: Int, e2)) = (t1, t2)
            i <= j
        })

        // println("ORIGINAL: " + iter)
        // println("ALL FOLDS: " + allFolds)
        // println("FOLDED: " + foldedExps)
        // println("SNIPPETS: " + snippets)
        // println("RESULT: " + res)

        /* Remove index tags and return optimized stBlock */
        res.map({ x => val (i, v) = x; v}).asInstanceOf[B]
    }

    def optimizeListAccess[B <: Traversable[_]](iter: B): B = {
        optimStBlock(iter, listAccessStrat[B])
    }
}