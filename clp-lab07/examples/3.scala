object HelloInt {
  def foo(n: Int): Int = {
    def plus1(m: Int): Int = { times(m + 1) }
    inline def times2(o: Int): Int = { 2 * o }
    plus1(times2(times2(n))) // inlined and cfed to 4 * n + 1
  }
  foo(1)
}
