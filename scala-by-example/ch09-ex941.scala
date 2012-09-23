def squareList(xs: List[Int]): List[Int] = xs match {
  case List() => Nil
  case y :: ys => (y*y) :: squareList(ys)
}

def squareList2(xs: List[Int]): List[Int] =
  xs map (x => x * x)
