object Foo {
  type T = {
    type U
    def bar : U
  }

  val baz : T = new {
    type U = String
    def bar : U = "Hello World!"
  }
}

def test(f: Foo.baz.U) = f
