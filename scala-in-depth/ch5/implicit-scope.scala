object companion {
  trait Foo
  object Foo {
    implicit val x: Foo = new Foo {
      override def toString = "Companion Foo"
    }
  }
}
implicitly[Foo]


object typeParameters {
  trait Bar
  object Bar {
    implicit val list: List[Bar] = List(new Bar {})
  }
}
implicitly[List[typeParameters.Bar]]


object typeClasses {
  trait BinaryFormat[T] {
    def asBinary(entity: T): Array[Byte]
  }

  trait Baz
  object Baz {
    implicit lazy val binaryFormat: BinaryFormat[Baz] = new BinaryFormat[Baz] {
      def asBinary(entity: Baz) = "serializedBaz".getBytes
    }
  }
}
implicitly[typeClasses.BinaryFormat[typeClasses.Baz]]


object nesting {
  trait Qux
  implicit def newQux: Qux = new Qux {
    override def toString = "Implicit qux"
  }
}
implicitly[nesting.Qux]


object obj {
  // ran out of metasyntactic variables that I know
  object Krikkit { override def toString = "Krikkit" }
  implicit def k: Krikkit.type = Krikkit
}
implicitly[obj.Krikkit.type]
