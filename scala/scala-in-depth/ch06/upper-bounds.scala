class A {
  type B <: Traversable[Int]
  def count(b: B) = b.foldLeft(0)(_+_)
}

def test {
  val x = new A { type B = List[Int] }
  println(x.count(List(1,2)))
  //x.count(Set(1,2))

  val y = new A { type B = Set[Int] }
  println(y.count(Set(1,2)))
}
