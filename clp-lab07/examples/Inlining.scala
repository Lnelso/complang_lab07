object Inlining {
  inline def inlinedSquare(i: Int): Int = {
    i * i
  }

  def test(): Int = {
    val a: Int = inlinedSquare(2);
    val b: Int = inlinedSquare(3);
    a
  }

  //Std.printString("5^2 = "  ++ Std.intToString(inlinedSquare(5)));
  //Std.printString("10^2 = "  ++ Std.intToString(inlinedSquare(10)));
  Std.printString(Std.intToString(test()))
}