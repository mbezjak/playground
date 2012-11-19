def spawn(p: => Unit) {
  val t = new Thread { override def run = p }
  t.start
}

class SyncVar[A] {
  private var isDefined: Boolean = false
  private var value: A = _

  def get: A = synchronized {
    while (!isDefined) wait()
    value
  }

  def set(x: A) = synchronized {
    value = x
    isDefined = true
    notifyAll
  }

  def isSet: Boolean = synchronized {
    isDefined
  }

  def unset = synchronized {
    isDefined = false
  }

}

def future[A](p: => A): (() => A) = {
  val result = new SyncVar[A]
  spawn { result set p }
  (() => result.get)
}

def par[A, B](p1: => A, p2: => B): (A, B) = (p1, future(p2)())

def replicate(start: Int, end: Int)(p: Int => Unit) {
  if (start == end) ()
  else if (start + 1 == end) p(start)
  else {
    val mid = (start + end) / 2
    par(replicate(start, mid)(p), replicate(mid, end)(p))
  }
}

def parMap[A, B : ClassManifest](f: A => B, xs: Array[A]): Array[B] = {
  val n       = xs.length
  val results = Array.ofDim[B](n)

  replicate(0, n) { i => results(i) = f(xs(i)) }

  results
}

class Lock {
  var available = true

  def acquire = synchronized {
    while(!available) wait()
    available = false
  }

  def release = synchronized {
    available = true
    notifyAll
  }
}

class ReadersWriters {
  val m = new MailBox

  private case class Writers(n: Int) { m send this }
  private case class Readers(n: Int) { m send this }
  Writers(0)
  Readers(0)

  def startRead: Unit = m receive {
    case Writers(n) if n == 0 => m receive {
      case Readers(n) =>
        Writers(0)
        Readers(n + 1)
    }
  }

  def startWrite: Unit = m receive {
    case Writers(n) =>
      Writers(n + 1)
      m receive { case Readers(n) if n == 0 => }
  }

  def endRead: Unit = m receive {
    case Readers(n) => Readers(n - 1)
  }

  def endWrite: Unit = m receive {
    case Writers(n) =>
      Writers(n - 1)
      if (n == 0) Readers(0)
  }
}

class LinkedList[A] {
  var elem: A = _
  var next: LinkedList[A] = null
}

class Channel[A] {
  private var written     = new LinkedList[A]
  private var lastWritten = written
  private var nreaders    = 0

  def write(x: A) = synchronized {
    lastWritten.elem = x
    lastWritten.next = new LinkedList[A]
    lastWritten = lastWritten.next
    if (nreaders > 0) notify
  }

  def read: A = synchronized {
    if (written.next == null) {
      nreaders += 1
      wait
      nreaders -= 1
    }

    val x = written.elem
    written = written.next
    x
  }
}

class SyncChannel[A] {
  private var data: A = _
  private var reading = false
  private var writing = false

  def write(x: A) = synchronized {
    while (writing) wait

    data = x
    writing = true

    if (reading) notifyAll
    else while (!reading) wait
  }

  def read: A = synchronized {
    while (reading) wait

    reading = true
    while (!writing) wait

    val x = data
    writing = false
    reading = false
    notifyAll
    x
  }
}

class ComputeServer(n: Int) {
  private abstract class Job {
    type T
    def task: T
    def ret(x: T)
  }

  private val openJobs = new Channel[Job]

  private def processor(i: Int) {
    while (true) {
      val job = openJobs.read
      job.ret(job.task)
    }
  }

  def future[A](p: => A): () => A = {
    val reply = new SyncVar[A]

    openJobs.write {
      new Job {
        type T = A
        def task = p
        def ret(x: A) = reply set x
      }
    }

    () => reply.get
  }

  spawn(replicate(0, n)(processor))
}

case object TIMEOUT

class MailBox {
  private abstract class Receiver {
    def isDefined(msg: Any): Boolean
    var msg: Any = _
  }

  private val sent = new LinkedList[Any]
  private var lastSent = sent
  private val receivers = new LinkedList[Receiver]
  private var lastReceiver = receivers

  def send(msg: Any) = synchronized {
    var r  = receivers
    var r1 = r.next

    while (r1 != null && !r1.elem.isDefined(msg)) {
      r  = r1
      r1 = r1.next
    }

    if (r1 != null) {
      r.next = r1.next
      r1.elem.msg = msg
      r1.elem.notify
    } else {
      lastSent = insert(lastSent, msg)
    }
  }

  def receive[A](f: PartialFunction[Any, A]): A = {
    val msg: Any = synchronized {
      var s  = sent
      var s1 = s.next

      while (s1 != null && !f.isDefinedAt(s1.elem)) {
        s  = s1
        s1 = s1.next
      }

      if (s1 != null) {
        s.next = s1.next
        s1.elem
      } else {
        var r = insert(lastReceiver, new Receiver {
          def isDefined(msg: Any) = f.isDefinedAt(msg)
        })
        lastReceiver = r
        r.elem.wait
        r.elem.msg
      }
    }

    f(msg)
  }

  def insert[A](l: LinkedList[A], x: A): LinkedList[A] = {
    l.next = new LinkedList[A]
    l.next.elem = x
    l.next.next = l.next
    l
  }
}

class OnePlaceBuffer {
  private val m = new MailBox

  private case class Empty()
  private case class Full(x: Int)
  m send Empty

  def write(x: Int) {
    m receive { case Empty => m send Full(x) }
  }

  def read: Int = m receive {
    case Full(x) =>
      m send Empty
      x
  }
}
