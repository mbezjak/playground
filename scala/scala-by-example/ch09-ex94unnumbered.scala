def forall[A](xs: List[A])(p: A => Boolean): Boolean =
  (xs filter p) == xs

def exists[A](xs: List[A])(p: A => Boolean): Boolean =
  !(xs filter p).isEmpty
