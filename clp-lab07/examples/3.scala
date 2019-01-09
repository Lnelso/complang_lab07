object HelloInt {
  def foo(n: Int): Int = {
    def a(n:Int):Int = {
      a(n+1)
    }
    a(n)
  }
  foo(1)
}
