object Inlining {
  inline def inlinedSquare(square: Int): Int = {
    square * square
  }

  def test(bullshit: Int): Int = {
    val a: Int = inlinedSquare(2);
    val b: Int = inlinedSquare(3);
    a
  }

  Std.printString("5^2 = "  ++ Std.intToString(inlinedSquare(5)));
  Std.printString("10^2 = "  ++ Std.intToString(inlinedSquare(10)));
  Std.printString("100^2 = "  ++ Std.intToString(inlinedSquare(100)));
  Std.printString("9^2 = "  ++ Std.intToString(inlinedSquare(9)));
  Std.printString("5^2 = "  ++ Std.intToString(inlinedSquare(5)));
  Std.printString(Std.intToString(test(3)))
}