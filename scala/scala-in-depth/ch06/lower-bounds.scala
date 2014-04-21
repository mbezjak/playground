class A {
  type B >: List[Int]
  def foo(b: B) = b
}

def test {
  val x = new A { type B = Traversable[Int] }
  println(x.foo(Set(1)))

  //val y = new A { type B = Set[Int] }
}
