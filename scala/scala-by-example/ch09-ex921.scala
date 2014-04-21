import annotation.tailrec

def length(xs: List[_]): Int = {
  @tailrec def iter(ys: List[_], count: Int): Int = ys match {
    case List() => count
    case y :: ys1 => iter(ys1, count+1)
  }

  iter(xs, 0)
}
