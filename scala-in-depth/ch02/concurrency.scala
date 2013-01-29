trait Service[K, V] {
  def lookup(k: K): Option[V]
  def insert(k: K, v: V): Unit
}

class MutableService[K, V] extends Service[K, V] {
  val index = new collection.mutable.HashMap[K, V]
  override def lookup(k: K): Option[V]  = synchronized { index lift k }
  override def insert(k: K, v: V): Unit = synchronized { index.put(k, v) }
}

class ImmutableService[K, V] extends Service[K, V] {
  var index = new collection.immutable.HashMap[K, V]
  // is lookup impl correct under JVM memory model?
  override def lookup(k: K): Option[V]  = index lift k
  override def insert(k: K, v: V): Unit = synchronized { index = index + ((k, v)) }
}


// quick and dirty
object Tester {
  import java.util.concurrent.Executors

  def run(s: Service[Int, Int]) {
    val pool = Executors.newFixedThreadPool(2)

    def submit(work: => Unit) {
      pool.submit(new Runnable() {
        override def run = work
      })
    }
    def iterate(work: Int => Unit) {
      for(x <- 1 to 1e6.toInt) { work(x) }
    }

    submit {
      iterate { x => s.insert(x, x) }
      println("done inserting")
    }

    submit(iterate { x => if (s.lookup(x).isEmpty) print('.') })
  }

  def testMutable   = run(new MutableService[Int, Int]())
  def testImmutable = run(new ImmutableService[Int, Int]())

}
