object holder {

  trait IntSet {
    def incl(x: Int): IntSet
    def contains(x: Int): Boolean
    def union(other: IntSet): IntSet
    def intersect(other: IntSet): IntSet
    def excl(x: Int): IntSet
    def isEmpty: Boolean
  }

  object EmptySet extends IntSet {
    override def contains(x: Int): Boolean = false
    override def incl(x: Int): IntSet = new NonEmptySet(x, this, this)
    override def union(other: IntSet): IntSet = other
    override def intersect(other: IntSet): IntSet = this
    override def excl(x: Int): IntSet = this
    override def isEmpty: Boolean = true
    override def toString = ""
  }

  class NonEmptySet(elem: Int, left: IntSet, right: IntSet) extends IntSet {
    override def contains(x: Int): Boolean =
      if (x < elem) left.contains(x)
      else if (x > elem) right.contains(x)
      else true

    override def incl(x: Int): IntSet =
      if (x < elem) new NonEmptySet(elem, left incl x, right)
      else if (x > elem) new NonEmptySet(elem, left, right incl x)
      else this

    override def union(other: IntSet): IntSet =
      left.union(right union other).incl(elem)

    override def intersect(other: IntSet): IntSet = {
      val l = left intersect other
      var r = right intersect other

      if (other contains elem) new NonEmptySet(elem, l, r)
      else l union r
    }

    override def excl(x: Int): IntSet =
      if (x < elem) new NonEmptySet(elem, left excl x, right)
      else if (x > elem) new NonEmptySet(elem, left, right excl x)
      else left union right

    override def isEmpty: Boolean = false

    override def toString = {
      def sub(s: IntSet) = "{" + s.toString + "}"
      "" + elem + ", left=" + sub(left) + ", right=" + sub(right)
    }
  }

}

import holder._

val s1 = EmptySet.incl(1).incl(2).incl(3).incl(9)
val s2 = EmptySet.incl(2).incl(3).incl(4).incl(0)

val union = s1 union s2
val intersect = s1 intersect s2
var excl = s1 excl 1 excl 9
