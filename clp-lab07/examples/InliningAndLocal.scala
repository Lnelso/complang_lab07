object InliningAndLocal {
  def foo(n: Int): Int = {
    def plus1(n: Int): Int = { n + 1 }
    inline def times2(n: Int): Int = { 2 * n }
    plus1(times2(times2(n))) // inlined and cf-ed to ‘4 * n + 1’
  }
}
