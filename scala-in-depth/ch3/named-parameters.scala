class Foo {
  def foo(one: Int = 1,
          two: String = "two",
          three: Double = 2.5): String = one + two + three
}

val x = new Foo()
x.foo()
x.foo(two = "not two")
x.foo(0, "zero", 0.1)
x.foo(4, three = 0.4)



class Parent {
  def foo(bar: Int = 1, baz: Int = 2): Int = bar + baz
}

class Child extends Parent {
  // confusing parameter names compared to super.foo
  override def foo(baz: Int = 3, bar: Int = 4): Int = super.foo(baz, bar)
}

val p = new Parent()
val c1 = new Child()
val c2: Parent = new Child()
p.foo()
c1.foo()
c2.foo()
c1.foo(bar = 1)
c2.foo(bar = 1)
