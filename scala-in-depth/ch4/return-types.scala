trait MessageDispatcher[-T] {
  def sendMessage(msg: T): Unit
}

trait OutputChannel[-T] {
  def ! (msg: T): Unit
}

class ActorDispatcher[-T, U <: OutputChannel[T]](receiver: U) extends MessageDispatcher[T] {
  override def sendMessage(msg: T) {
    receiver ! msg
  }
}

object NullDispatcher extends MessageDispatcher[Any] {
  override def sendMessage(msg: Any): Unit = {}
}

object MyFactory {
  def createDispatcher(a: OutputChannel[Any]): MessageDispatcher[Any] = a match {
    case actor: OutputChannel[Any] => new ActorDispatcher(actor)
    case _ => NullDispatcher
  }
}
