// just to satisfy type checks

object x extends Seq[Int] {
  def iterator: Iterator[Int] = Iterator(42)
  def apply(idx: Int): Int = 42
  def length: Int = 1
}

case class Walker[T, DB](t: T, db: DB) extends Seq[T] {
  def iterator: Iterator[T] = Iterator(t)
  def apply(idx: Int): T = t
  def length: Int = 1
}

def sort[T, C](xs: C)(implicit ev : C <:< Seq[T]): C = xs

def test1 = sort(List(1,2,3))
def test2 = sort(x)
def test3 = sort(Walker(42, "ifx"))
