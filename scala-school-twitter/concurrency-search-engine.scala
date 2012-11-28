import java.util.concurrent.{ConcurrentHashMap, BlockingQueue, LinkedBlockingQueue, Executors}
import scala.collection.JavaConversions._
import scala.collection.mutable
import scala.io.Source

case class User(name: String, id: Int)

class InvertedIndex(val userMap: mutable.Map[String,User]) {
  def this() = this(new mutable.HashMap[String, User])

  def tokenizeName(name: String): Seq[String] =
    name split " " map (_.toLowerCase)

  def add(term: String, user: User) {
    userMap += term -> user
  }

  def add(user: User) {
    // tokenizeName was measured to be the most expensive operation
    val tokens = tokenizeName(user.name)

    tokens.foreach { term =>
      userMap.synchronized { add(term, user) }
    }
  }
}

class ConcurrentInvertedIndex(userMap: mutable.ConcurrentMap[String,User]) extends InvertedIndex(userMap) {
  def this() = this(new ConcurrentHashMap[String,User])
}


// Concrete producer
class Producer(path: String, queue: BlockingQueue[String]) extends Runnable {
  override def run() {
    Source.fromFile(path, "utf-8").getLines.foreach { line =>
      queue put line
    }
  }
}

// Abstract consumer
abstract class Consumer(queue: BlockingQueue[String]) extends Runnable {
  override def run() {
    while (true) {
      val item = queue.take()
      consume(item)
    }
  }

  def consume(x: String)
}

trait UserMaker {
  def makeUser(line: String) = line split "," match {
    case Array(name, userid) => User(name, userid.trim.toInt)
  }
}

class IndexerConsumer(index: InvertedIndex, queue: BlockingQueue[String]) extends Consumer(queue) with UserMaker {
  def consume(x: String) = index.add(makeUser(x))
}


// Setup
val index    = new ConcurrentInvertedIndex()
val queue    = new LinkedBlockingQueue[String]()
val producer = new Producer("users.txt", queue)
val cores    = 4
val pool     = Executors.newFixedThreadPool(cores)

// One thread for the producer
new Thread(producer).start()

// One consumer per core
for (i <- 1 to cores) {
  pool.submit(new IndexerConsumer(index, queue))
}


// Result
Thread.sleep(2000)
println("index = " + index.userMap)
