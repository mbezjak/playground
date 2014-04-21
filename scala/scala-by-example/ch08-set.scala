object holder {

  abstract class Set[A <% Ordered[A]] {
    def incl(x: A): Set[A]
    def contains(x: A): Boolean
  }

  class EmptySet[A <% Ordered[A]] extends Set[A] {
    def incl(x: A) = new NonEmptySet(x, this, this)
    def contains(x: A) = false
  }

  class NonEmptySet[A <% Ordered[A]](elem: A, left: Set[A], right: Set[A]) extends Set[A] {
    def incl(x: A) =
      if (x < elem) new NonEmptySet(elem, left incl x, right)
      else if (x > elem) new NonEmptySet(elem, left, right incl x)
      else this

    def contains(x: A) =
      if (x < elem) left contains x
      else if (x > elem) right contains x
      else true
  }

  case class Num(value: Double) extends Ordered[Num] {
    def compare(that: Num) =
      if (this.value < that.value) -1
      else if (this.value > that.value) 1
      else 0
  }

}

import holder._
