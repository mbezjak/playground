def msort[A <% Ordered[A]](xs: List[A]): List[A] = {
  def merge(ys: List[A], zs: List[A]): List[A] = {
    if (ys.isEmpty) zs
    else if (zs.isEmpty) ys
    else if (ys.head < zs.head) ys.head :: merge(ys.tail, zs)
    else zs.head :: merge(ys, zs.tail)
  }

  if (xs.length <= 1) xs
  else {
    val (ys, zs) = xs.splitAt(xs.length / 2)
    merge(msort(ys), msort(zs))
  }
}

def msort2[A](less: (A,A) => Boolean)(xs: List[A]): List[A] = {
  def merge(xs1: List[A], xs2: List[A]): List[A] =
    if (xs1.isEmpty) xs2
    else if (xs2.isEmpty) xs1
    else if (less(xs1.head, xs2.head)) xs1.head :: merge(xs1.tail, xs2)
    else xs2.head :: merge(xs1, xs2.tail)

  val n = xs.length / 2
  if (n == 0) xs
  else merge(msort2(less)(xs take n), msort2(less)(xs drop n))
}

val intSort = msort2((x: Int, y: Int) => x < y)_
val reverseSort = msort2((x: Int, y: Int) => x > y)_
