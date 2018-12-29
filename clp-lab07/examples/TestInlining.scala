object TestInlining {
  inline def abs(n: Int): Int = {
    if (n < 0){
      -n
    }
    else{
      n
    }
  }

  val a: Int = abs(123);
  val b: Int = abs(-456);

  Std.printString(Std.intToString(a));
  Std.printString(Std.intToString(b))
}