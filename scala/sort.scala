object immutable {

  def isort[A <% Ordered[A]](xs: List[A]): List[A] = {
    def insert(y: A, ys: List[A]): List[A] = ys match {
      case Nil     => List(y)
      case z :: zs => if (y <= z) y :: ys else z :: insert(y, zs)
    }

    xs match {
      case Nil     => Nil
      case y :: ys => insert(y, isort(ys))
    }
  }

  def msort[A <% Ordered[A]](xs: List[A]): List[A] = {
    def merge(xs1: List[A], xs2: List[A]): List[A] = (xs1, xs2) match {
      case (Nil, ys) => ys
      case (ys, Nil) => ys
      case (y :: ys, z :: zs) =>
        if (y < z) y :: merge(ys, xs2) else z :: merge(xs1, zs)
    }

    val n = xs.length / 2
    if (n == 0) xs
    else {
      val (fst, snd) = xs splitAt n
      merge(msort(fst), msort(snd))
    }
  }

  def qsort[A <% Ordered[A]](xs: List[A]): List[A] = xs match {
    case Nil | List(_) => xs
    case y :: ys =>
      qsort(ys filter (_ <= y)) ++ List(y) ++ qsort(ys filter (_ > y))
  }

  def qsort[A <% Ordered[A] : ClassManifest](xs: Array[A]): Array[A] =
    if (xs.length <= 1) xs
    else {
      val pivot = xs(xs.length / 2)
      Array.concat(
        qsort(xs filter (_ < pivot)),
              xs filter (_ == pivot),
        qsort(xs filter (_ > pivot)))
    }

}

object mutable {

  def qsort[A <% Ordered[A]](xs: Array[A]) {
    def swap(i: Int, j: Int) {
      val t = xs(i)
      xs(i) = xs(j)
      xs(j) = t
    }
    def sort1(l: Int, r: Int) {
      val pivot = xs((l + r) / 2)
      var i = l
      var j = r
      while (i <= j) {
        while (xs(i) < pivot) i += 1
        while (xs(j) > pivot) j -= 1
        if (i <= j) {
          swap(i, j)
          i += 1
          j -= 1
        }
      }
      if (l < j) sort1(l, j)
      if (j < r) sort1(i, r)
    }

    sort1(0, xs.length - 1)
  }

}

val l =  List(8, 3, 1, -1, 9, 2, 5, 0, 7, 6, -2, 4)
val a = Array(8, 3, 1, -1, 9, 2, 5, 0, 7, 6, -2, 4)
