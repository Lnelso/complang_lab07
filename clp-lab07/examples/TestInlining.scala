object TestInlining {
  def abs(n: Int): Int = {
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

  def test(n: Int): Int ={
    times2(times2(n+1))
  }

  val a: Int = abs(123);
  val b: Int = abs(-456);
  val c: Int = times2(3);

  Std.printString(Std.intToString(a));
  Std.printString(Std.intToString(b));
  Std.printString(Std.intToString(c))
}