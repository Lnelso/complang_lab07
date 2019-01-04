object TestInlining {
  inline def abs(n: Int): Int = {
    if (n < 0){
      -n
    }
    else{
      n
    }
  }

  inline def times2(n: Int): Int = {
    2 * n
  }

  inline def test(n: Int): Int ={
    2 + 2 + times2(times2(n+1))
  }

  val a: Int = abs(123);
  val b: Int = abs(-456);
  val c: Int = times2(3);
  val d: Int = test(1);

  Std.printString(Std.intToString(a));
  Std.printString(Std.intToString(b));
  Std.printString(Std.intToString(c));
  Std.printString(Std.intToString(d))
}