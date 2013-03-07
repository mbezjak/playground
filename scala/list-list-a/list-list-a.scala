import scalaz._
import Scalaz._

case class LL[A](run: List[List[A]]) {
  def map[B](f: A => B): LL[B] =
    LL(run.map (_ map f))

  def flatMap[B](f: A => LL[B]): LL[B] =
    LL(run.flatMap { a =>
      a.traverse(x => f(x).run) map (_.flatten)
    })

  def mplus(b: => LL[A]): LL[A] =
    LL(run ++ b.run)


  // seems equivalent to MonadPlus.filter
  def filter(p: A => Boolean): LL[A] =
    LL(run map (_ filter p))

  def withFilter(p: A => Boolean): LL[A] =
    filter(p)
}

object LL {
  implicit val monad: MonadPlus[LL] = new MonadPlus[LL] {
    def point[A](a: => A): LL[A] = LL(List(List(a)))
    def bind[A,B](fa: LL[A])(f: A => LL[B]): LL[B] = fa flatMap f
    def empty[A]: LL[A] = LL(Nil)
    def plus[A](a: LL[A], b: => LL[A]): LL[A] = a mplus b
  }
}


object Main extends App {
  val ll  = LL(List(
    List(1, 2, 3),
    List(4, 5, 6),
    List(7, 8, 9)
  ))
  val ll2 = LL(List(List(100), List(101), List(102)))
  val ll3 = LL(List(List(500), List(501), List(502)))

  val k: Int => LL[Int] = x => LL(List(List(x,x)))

  printf("v map (+1)   = %s\n", ll map (_ + 1))
  printf("v flatMap k  = %s\n", ll flatMap k)

  printf("mzero >>= k  = %s\n", MonadPlus[LL].empty flatMap k)
  printf("v >> mzero   = %s\n", ll flatMap { _ => MonadPlus[LL].empty[Int] })
  printf("mzero + v    = %s\n", MonadPlus[LL].empty mplus ll)
  printf("v + mzero    = %s\n", ll mplus MonadPlus[LL].empty)
  printf("(a + b) + c  = %s\n", (ll mplus ll2) mplus ll3)
  printf("a + (b + c)  = %s\n", ll mplus (ll2 mplus ll3))

  printf("for comp     = %s\n", for (x <- ll) yield x * 2)
  printf("for if comp  = %s\n", for (x <- ll if x < 6) yield x)
}
