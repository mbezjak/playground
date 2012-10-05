object holder {

  /*
  implicit def int2ordered(x: Int): Ordered[Int] = new Ordered[Int] {
    def compare(y: Int): Int =
      if (x < y) -1
      else if (x > y) 1
      else 0
  }
  */

  def sort[A <% Ordered[A]](xs: List[A]): List[A] =
    if (xs.isEmpty || xs.tail.isEmpty) xs
    else {
      val (ys, zs) = xs.splitAt(xs.length / 2)
      merge(sort(ys), sort(zs))
    }

  def merge[A <% Ordered[A]](xs1: List[A], xs2: List[A]): List[A] =
    if (xs1.isEmpty) xs2
    else if (xs2.isEmpty) xs1
    else if (xs1.head < xs2.head) xs1.head :: merge(xs1.tail, xs2)
    else xs2.head :: merge(xs1, xs2.tail)

}

import holder._
