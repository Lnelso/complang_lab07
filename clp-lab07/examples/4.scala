object HelloInt {
  def foo(n: Int): Int = {
    def a(n:Int):Int = {
      if(2 < n) {n} else {a(n+1)}
    }
  a(n)
  }
}
