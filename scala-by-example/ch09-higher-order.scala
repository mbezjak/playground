def map[A,B](xs: List[A])(f: A => B): List[B] = xs match {
  case List()  => Nil
  case y :: ys => f(y) :: map(ys)(f)
}

def foreach[A](xs: List[A])(f: A => Unit): Unit = xs match {
  case List()  => Nil
  case y :: ys => f(y); foreach(ys)(f)
}

def filter[A](xs: List[A])(p: A => Boolean): List[A] = xs match {
  case List() => Nil
  case y :: ys => if (p(y)) y :: filter(ys)(p) else filter(ys)(p)
}

def forall[A](xs: List[A])(p: A => Boolean): Boolean =
  xs.isEmpty || (p(xs.head) && forall(xs.tail)(p))

def exists[A](xs: List[A])(p: A => Boolean): Boolean =
  !xs.isEmpty && (p(xs.head) || exists(xs.tail)(p))

def range(from: Int, to: Int): List[Int] =
  if (from >= to) Nil else from :: range(from+1, to)

def foldLeft[A, B](xs: List[A])(z: B)(op: (B, A) => B): B = xs match {
  case List()  => z
  case y :: ys => foldLeft(ys)(op(z, y))(op)
}

def reduceLeft[A](xs: List[A])(op: (A, A) => A): A = xs match {
  case List() => sys.error("reduceLeft(Nil)")
  case y :: ys => foldLeft(ys)(y)(op)
}

def sum(xs: List[Int]) = reduceLeft(0 :: xs)((x, y) => x + y)
def product(xs: List[Int]) = reduceLeft(1 :: xs)((x, y) => x * y)

def foldRight[A, B](xs: List[A])(z: B)(op: (A, B) => B): B = xs match {
  case List()  => z
  case y :: ys => op(y, foldRight(ys)(z)(op))
}

def reduceRight[A](xs: List[A])(op: (A, A) => A): A = xs match {
  case List() => sys.error("reduceRight(Nil)")
  case y :: Nil => y
  case y :: ys => op(y, reduceRight(ys)(op))
}

def sum2(xs: List[Int]) = reduceRight(0 :: xs)((x, y) => x + y)
def product2(xs: List[Int]) = reduceRight(1 :: xs)((x, y) => x * y)

def reverse[A](xs: List[A]): List[A] =
  ((Nil: List[A]) /: xs) {(xs, x) => x :: xs}

def flatMap[A,B](xs: List[A])(f: A => List[B]): List[B] =
  map(xs)(f).flatten
