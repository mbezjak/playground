object UseSimpleJavaClass extends App {
  val x = SimpleJavaClass.create("One")
  println(x.getName())

  val y = new SimpleJavaClass("Two")
  println(y.getName())
}
