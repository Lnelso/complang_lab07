object TestLists {
  inline def inlinedSquare(i: Int): Int = {
    i * i
  }

  Std.printString("5^2 = "  ++ Std.intToString(inlinedSquare(5)));
  Std.printString("10^2 = "  ++ Std.intToString(inlinedSquare(10)))
}