import scalaz._
import Scalaz._
import java.io._

trait ManagedResource[T] {
  def loan[U](f: T => U): U
}

object ManagedResource {
  type MR[T] = ManagedResource[T]

  object functor extends Functor[MR] {
    override def map[A, B](fa: MR[A])(f: A => B): MR[B] = new MR[B] {
      override def loan[U](l: B => U): U = fa.loan(f andThen l)
    }
  }

  // the only implicit because a Monad is a Functor
  implicit object monad extends Monad[MR] {
    override def point[A](a: => A): MR[A] = ManagedResource(a)
    override def bind[A, B](fa: MR[A])(f: A => MR[B]): MR[B] = new MR[B] {
      override def loan[U](l: B => U): U = fa.loan(a => f(a).loan(l))
    }
  }

  def apply[A](a: A): MR[A] = new MR[A] {
    override def loan[U](f: A => U): U = f(a)
  }
}

object IO {
  type LazyTraversable[T] = collection.TraversableView[T, Traversable[T]]

  def readFile(file: File): ManagedResource[InputStream] = new ManagedResource[InputStream] {
    override def loan[U](f: InputStream => U): U = {
      val stream = new FileInputStream(file)
      try {
        f(stream)
      } finally {
        stream.close()
      }
    }
  }

  def writeFile(file: File): ManagedResource[BufferedWriter] = new ManagedResource[BufferedWriter] {
    override def loan[U](f: BufferedWriter => U): U = {
      val stream   = new FileOutputStream(file)
      val writer   = new OutputStreamWriter(stream)
      val buffered = new BufferedWriter(writer)
      try {
        f(buffered)
      } finally {
        buffered.close()
      }
    }
  }

  def makeLineTraversable(input: BufferedReader): LazyTraversable[String] = new Traversable[String] {
    override def foreach[U](f: String => U): Unit = {
      var line = input.readLine()
      while (line != null) {
        f(line)
        line = input.readLine()
      }
    }
  }.view

  def getLines(file: File): ManagedResource[LazyTraversable[String]] =
    for {
      input <- readFile(file)
      val reader   = new InputStreamReader(input)
      val buffered = new BufferedReader(reader)
    } yield makeLineTraversable(buffered)

  def lineLengthCount(inFile: File, outFile: File): ManagedResource[Unit] =
    for {
      lines <- getLines(inFile)
      val counts = lines.map(_.length).toSeq.zipWithIndex
      writer <- writeFile(outFile)
    } yield writer.write(counts.mkString("\n"))

}

object test {
  def workflow = IO.lineLengthCount(new File("mr.scala"), new File("counts.txt"))
  def work     = workflow.loan(identity)
}
