import collection.mutable

class Matrix(private val repr: Array[Array[Double]]) {
  def row(idx: Int): Seq[Double] = repr(idx)
  def col(idx: Int): Seq[Double] = repr.foldLeft(mutable.ArrayBuffer[Double]()) {
    (buffer, currentRow) =>
      buffer.append(currentRow(idx))
      buffer
  } toArray

  lazy val rowRank = repr.size
  lazy val colRank = if (rowRank > 0) repr(0).size else 0

  override def toString = "Matrix" + repr.foldLeft("") {
    (msg, row) => msg + row.mkString("\n|", " | ", "|")
  }
}

trait ThreadStrategy {
  def execute[A](func: Function0[A]): Function0[A]
}

object SameThreadStrategy extends ThreadStrategy {
  def execute[A](func: Function0[A]) = func
}

object ThreadPoolStrategy extends ThreadStrategy {
  import java.util.concurrent.{Callable, Executors}

  val pool = Executors.newFixedThreadPool(Runtime.getRuntime.availableProcessors)

  def execute[A](func: Function0[A]) = {
    val future = pool.submit(new Callable[A] {
      def call(): A = {
        println("Executing function on thread: " + Thread.currentThread.getName)
        func()
      }
    })

    () => future.get()
  }
}

object MatrixUtils {
  def multiply(a: Matrix, b: Matrix)(implicit threading: ThreadStrategy = SameThreadStrategy): Matrix = {
    assert(a.colRank == b.rowRank)

    val buffer = new Array[Array[Double]](a.rowRank)
    for (i <- 0 until a.rowRank) {
      buffer(i) = new Array[Double](b.colRank)
    }

    def computeValue(row: Int, col: Int): Unit = {
      val pairwiseElements = a.row(row).zip(b.col(col))
      val products         = for ((x, y) <- pairwiseElements) yield x*y
      buffer(row)(col)     = products.sum
    }

    val computations = for {
      i <- 0 until a.rowRank
      j <- 0 until b.colRank
    } yield threading.execute { () => computeValue(i,j) }

    computations.foreach(_())
    new Matrix(buffer)
  }

}

val x = new Matrix(Array(
  Array(1.0, 2.0, 3.0),
  Array(2.0, 3.0, 4.0)
))

val y = new Matrix(Array(
  Array(5.0, 6.0),
  Array(7.0, 8.0),
  Array(9.0, 9.0)
))

def plain() {
  println(MatrixUtils.multiply(x, y))
}

def conc() {
  implicit val s = ThreadPoolStrategy
  println(MatrixUtils.multiply(x, y))
}
