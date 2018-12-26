object TestLists {
  val l: List = l.Cons(5, l.Cons(-5, l.Cons(-1, l.Cons(0, l.Cons(10, l.Nil())))));
  Std.printString(l.toString(l.concat(l.Cons(1, l.Cons(2, l.Nil())), l.Cons(3, l.Nil()))));
  Std.printInt(l.sum());
  Std.printString(l.toString(L.mergeSort(l)))
}
