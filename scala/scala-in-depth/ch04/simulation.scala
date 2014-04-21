trait SimulationMessage
trait SimulationResponse
trait SimulationContext {
  def respond(sender: String, response: SimulationResponse): Unit
}

case class EchoContext() extends SimulationContext {
  override def respond(sender: String, response: SimulationResponse): Unit =
    println("response from sender: " + sender)
}

trait SimulationEntity {
  def handleMessage(msg: SimulationMessage,
                    ctx: SimulationContext): Unit
}

trait MixableParent extends SimulationEntity {
  override def handleMessage(msg: SimulationMessage,
                             ctx: SimulationContext): Unit = {}
}

case class PingRequest(ip: String, sender: String) extends SimulationMessage
case class PingResponse(mac: String) extends SimulationResponse
trait NetworkEntity extends MixableParent {
  def getMacAddress(ip: String): String
  def hasIpAddress(addr: String): Boolean

  override def handleMessage(msg: SimulationMessage,
                             ctx: SimulationContext): Unit = msg match {
      case PingRequest(ip, sender) if hasIpAddress(ip) =>
        ctx respond (sender, PingResponse(getMacAddress(ip)))
      case _ => super.handleMessage(msg, ctx)
    }
}

case class Test(x: String) extends SimulationMessage
trait Router extends SimulationEntity {
  def getMacAddress(ip: String)  = "FF:12"
  def hasIpAddress(addr: String) = true

  override def handleMessage(msg: SimulationMessage,
                             ctx: SimulationContext): Unit = msg match {
    case Test(x) => println("YAY! " + x)
    case _ =>
  }
}

// class linearization can be seen in "Linear Supertypes" in scaladoc
// $ scaladoc simulation.scala
class A extends Router with NetworkEntity
class B extends MixableParent with Router with NetworkEntity
