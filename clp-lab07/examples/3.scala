object HelloInt {
  def foo(n: Int): Int = {
    def a(n:Int):Int = {
      if(7 < n) {n} else {a(n+1)}
    }
    a(n)
  }
  foo(1)
}
