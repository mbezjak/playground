object holder {

  trait Iterator[+A] {
    def hasNext: Boolean
    def next: A

    def buffered: BufferedIterator[A] = new BufferedIterator[A] {
      private var cur: Option[A] = forward
      private def forward =
        if (Iterator.this.hasNext) Some(Iterator.this.next) else None

      def hasNext = cur.isDefined
      def next = { val x = head; cur = forward; x }
      def head = cur.get
    }

    def append[B >: A](that: Iterator[B]): Iterator[B] = new Iterator[B] {
      def hasNext = Iterator.this.hasNext || that.hasNext
      def next = if (Iterator.this.hasNext) Iterator.this.next else that.next
    }

    def map[B](f: A => B): Iterator[B] = new Iterator[B] {
      def hasNext = Iterator.this.hasNext
      def next = f(Iterator.this.next)
    }

    def flatMap[B](f: A => Iterator[B]): Iterator[B] = new Iterator[B] {
      private var cur: Iterator[B] = Iterator.empty
      def hasNext =
        if (cur.hasNext) true
        else if (Iterator.this.hasNext) { cur = f(Iterator.this.next); hasNext }
        else false
      def next =
        if (cur.hasNext) cur.next
        else if (Iterator.this.hasNext) { cur = f(Iterator.this.next); next }
        else sys.error("next on empty iterator")
    }

    def foreach(f: A => Unit): Unit =
      while (hasNext) f(next)

    def filter(p: A => Boolean): Iterator[A] = new BufferedIterator[A] {
      private val source = Iterator.this.buffered
      private def skip = {
        while (source.hasNext && !p(source.head)) source.next
      }
      def hasNext = { skip; source.hasNext }
      def next = { skip; source.next }
      def head = { skip; source.head }
    }

    def zip[B](that: Iterator[B]): Iterator[(A, B)] = new Iterator[(A, B)] {
      def hasNext = Iterator.this.hasNext && that.hasNext
      def next = (Iterator.this.next, that.next)
    }

  }

  object Iterator {

    object empty extends Iterator[Nothing] {
      def hasNext = false
      def next = sys.error("next on empty iterator")
    }

    def fromArray[A](xs: Array[A]): Iterator[A] = new Iterator[A] {
      private var i = 0
      def hasNext = i < xs.length
      def next = {
        if (i < xs.length) { val x = xs(i); i += 1; x }
        else sys.error("next on empty iterator")
      }
    }

    def range(start: Int, end: Int): Iterator[Int] = new Iterator[Int] {
      private var current = start
      def hasNext = current < end
      def next = {
        var r = current
        if (current < end) current += 1
        else sys.error("end of iterator")
        r
      }
    }

    def from(start: Int): Iterator[Int] = new Iterator[Int] {
      private var last = start - 1
      def hasNext = true
      def next = { last += 1; last }
    }

  }

  trait BufferedIterator[+A] extends Iterator[A] {
    def head: A
  }

}

import holder._
import holder.Iterator

val i1 = Iterator.fromArray(Array(1, 2, 3))
val i2 = Iterator.range(4, 10)

def greater(xs: Array[Int], limit: Int) =
  Iterator.fromArray(xs).
          zip(Iterator.from(0)).
          filter { case (x,i) => x > limit }.
          map { case (x,i) => i }

def greater2(xs: Array[Int], limit: Int) =
  for { (x,i) <- Iterator.fromArray(xs).zip(Iterator.from(0))
        if x > limit
      } yield i
