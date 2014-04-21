object holder {

  class Symbols(val compiler: Compiler) {
    import compiler.types._

    val Add = new Symbol("+", FunType(List(IntType, IntType), IntType))
    val Sub = new Symbol("-", FunType(List(IntType, IntType), IntType))

    class Symbol(name: String, tpe: Type) {
      override def toString = name + ": " + tpe
    }
  }

  class Types(val compiler: Compiler) {
    import compiler.symtab._

    abstract class Type
    case class FunType(args: List[Type], res: Type) extends Type
    case class NamedType(sym: Symbol) extends Type
    case object IntType extends Type
  }

  class Compiler {
    lazy val symtab = new Symbols(this)
    lazy val types  = new Types(this)
  }

}

import holder._
