import annotation.tailrec

object holder {

  abstract class Integer {
    def isZero: Boolean
    def isPositive: Boolean
    def negate: Integer
    def predecessor: Integer
    def successor: Integer
    def +(that: Integer): Integer
    def -(that: Integer): Integer
  }

  object Zero extends Integer {
    def isZero: Boolean = true
    def isPositive: Boolean = true
    def negate: Integer = this
    def predecessor: Integer = new Pred(Zero)
    def successor: Integer = new Succ(Zero)
    def +(that: Integer): Integer = that
    def -(that: Integer): Integer = that.negate

    override def toString = "Zero"
  }

  class Succ(x: Integer) extends Integer {
    def isZero: Boolean = false
    def isPositive: Boolean = true
    def negate: Integer = {
      @tailrec def back(p: Integer, n: Integer): Integer =
        if (p.isZero) n
        else back(p.predecessor, n.predecessor)

      back(this, Zero)
    }
    def predecessor: Integer = x
    def successor: Integer = new Succ(this)
    def +(that: Integer): Integer = x + that.successor
    def -(that: Integer): Integer = this + that.negate

    override def toString = "Succ(" + x.toString + ")"
  }

  class Pred(x: Integer) extends Integer {
    def isZero: Boolean = false
    def isPositive: Boolean = false
    def negate: Integer = {
      @tailrec def forward(n: Integer, p: Integer): Integer =
        if (n.isZero) p
        else forward(n.successor, p.successor)

      forward(this, Zero)
    }
    def predecessor: Integer = new Pred(this)
    def successor: Integer = x
    def +(that: Integer): Integer =
      if (that.isPositive) that + this
      else predecessor + that.successor
    def -(that: Integer): Integer =
      if (that.isPositive) predecessor - that.predecessor
      else that.negate + this

    override def toString = "Pred(" + x.toString + ")"
  }

}

import holder._

var two   = Zero.successor.successor
val three = Zero.successor.successor.successor
val mtwo  = Zero.predecessor.predecessor

println("2+3=%s".format(two + three))
println("2-3=%s".format(two - three))
println("2+(-2)=%s".format(two + mtwo))
println("2-(-2)=%s".format(two - mtwo))

println("(-2)+3=%s".format(mtwo + three))
println("(-2)-3=%s".format(mtwo - three))
println("(-2)+(-2)=%s".format(mtwo + mtwo))
println("(-2)-(-2)=%s".format(mtwo - mtwo))
