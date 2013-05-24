trait Observable {
  type Handle

  var callbacks : Map[Handle, this.type => Unit] = Map()

  def observe(callback: this.type => Unit): Handle = {
    val handle = createHandle(callback)
    callbacks += (handle -> callback)
    handle
  }

  def unobserve(handle: Handle) : Unit = {
    callbacks -= handle
  }

  protected def createHandle(callback: this.type => Unit): Handle

  protected def notifyListeners(): Unit =
    for(callback <- callbacks.values) callback(this)
}

trait DefaultHandles extends Observable {
  type Handle = (this.type => Unit)

  protected def createHandle(callback: this.type => Unit): Handle = callback
}

class IntStore(private var value: Int) extends Observable with DefaultHandles {
  def get: Int = value
  def set(newValue: Int): Unit = {
    value = newValue
    notifyListeners()
  }

  override def toString: String = "IntStore(" + value + ")"
}

def test1 {
  val x = new IntStore(5)

  val handle = x.observe(println)
  x.set(2)

  x.unobserve(handle)
  x.set(4)
}

def test2 {
  val x = new IntStore(5)
  val y = new IntStore(2)

  val callback = println(_ : Any)
  val handleX = x.observe(callback)
  val handleY = y.observe(callback)
  println(handleX == handleY)

//  y.unobserve(handleX)
}
