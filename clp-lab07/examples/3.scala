object HelloInt {
  def foo(n: Int): Int = {
    def plus1(n: Int): Int = { n + 1 }
    plus1(n)
  }
  foo(1)
}
