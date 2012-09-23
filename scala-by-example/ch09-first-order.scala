def isEmpty(xs: List[_]): Boolean = xs match {
  case List() => true
  case _ => false
}

def head[A](xs: List[A]): A = xs match {
  case Nil => sys.error("Nil.head")
  case x :: xs1 => x
}

def tail[A](xs: List[A]): List[A] = xs match {
  case Nil => sys.error("Nil.tail")
  case x :: xs1 => xs1
}

def length(xs: List[_]): Int = xs match {
  case Nil => 0
  case x :: xs1 => 1 + length(xs1)
}

def last[A](xs: List[A]): A = xs match {
  case Nil => sys.error("Nil.last")
  case x :: Nil => x
  case x :: xs1 => last(xs1)
}

def init[A](xs: List[A]): List[A] = xs match {
  case Nil => sys.error("Nil.init")
  case x :: Nil => Nil
  case x :: xs1 => x :: init(xs1)
}

def take[A](xs: List[A], n: Int): List[A] =
  if (n == 0 || isEmpty(xs)) Nil else head(xs) :: take(tail(xs), n-1)

def drop[A](xs: List[A], n: Int): List[A] =
  if (n == 0 || isEmpty(xs)) xs else drop(tail(xs), n-1)

def split[A](xs: List[A], n: Int): (List[A], List[A]) =
  (take(xs, n), drop(xs, n))

def atIndex[A](xs: List[A], n: Int): A = head(drop(xs, n))

def sublist[A](xs: List[A], n: Int, m: Int): List[A] =
  take(drop(xs, n), m-n)

def zip[A, B](xs: List[A], ys: List[B]): List[(A, B)] = {
  if (isEmpty(xs) || isEmpty(ys)) Nil
  else (head(xs), head(ys)) :: zip(tail(xs), tail(ys))
}

def concat[A](xs: List[A], ys: List[A]): List[A] = xs match {
  case List() => ys
  case x :: xs1 => x :: concat(xs1, ys)
}

def reverse[A](xs: List[A]): List[A] = xs match {
  case List() => Nil
  case y :: ys => concat(reverse(ys), List(y))
}
