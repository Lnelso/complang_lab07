object HelloInt {
  def foo(n: Int): Int = {
def plus1(n: Int): Int = { if(4 < n) {n} else { plus1(n + 1)} }
plus1(n) // inl
  }
  foo(1)
}
