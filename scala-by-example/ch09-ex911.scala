def insert(x: Int, xs: List[Int]): List[Int] = {
  if (xs.isEmpty) List(x)
  else if (x <= xs.head) x :: xs
  else xs.head :: insert(x, xs.tail)
}

def isort(xs: List[Int]): List[Int] = {
  if (xs.isEmpty) xs
  else insert(xs.head, isort(xs.tail))
}

def insert2(x: Int, xs: List[Int]): List[Int] = xs match {
  case List()  => List(x)
  case y :: ys => if (x <= y) x :: xs else y :: insert2(x, ys)
}

def isort2(xs: List[Int]): List[Int] = xs match {
  case List()  => Nil
  case y :: ys => insert2(y, isort2(ys))
}
