import org.graalvm.polyglot._;

object TestMain {
  def main(args: Array[String]) {
    var context = Context.newBuilder().allowIO(true).build();
    var array = context.eval("python", "[1,2,42,4]");
    var result = array.getArrayElement(2).asInt();
    System.out.println(result);

    var polyglot = Context.newBuilder().allowNativeAccess(true).build();
    var func = polyglot.eval("js", "(function(a){a.length})");
    System.out.println(func);

    if(polyglot.eval("ruby", "52").isBoolean())
      System.out.println( "Ruby boolean!");

    Base.test()
    Lisp.test()
    Pink.test()
    Pink_CPS.test()
    Pink_clambda.test()
    Matcher.test()
//    Bench.test()
    println("DONE")
  }
}
