def mapFun[A, B](xs: List[A], f: A => B): List[B] =
  (xs :\ (Nil: List[B])) {(x, xs) => f(x) :: xs}

def lengthFun[A](xs: List[A]): Int =
  (0 /: xs) {(xs, x) => xs + 1}
