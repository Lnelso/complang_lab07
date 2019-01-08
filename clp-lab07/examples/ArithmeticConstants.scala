object ArithmeticConstants {

  def test(): Int = {
    val b: Int = 5 + 3 + 2;
    b
  }

  inline def testBis(): Int = {
    val b: Int = 5 + 3 + 2;
    b
  }

  Std.printInt(test())
}
