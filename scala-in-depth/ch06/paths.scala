class Outer {
  trait Inner
  def in = new Inner {}

  def foo(o: this.Inner) = null
  def bar(o: Outer#Inner) = null
}

val x = new Outer
val y = new Outer

println(x.in)
println(y.in)

x.foo(x.in)
//x.foo(y.in)
x.bar(y.in)
