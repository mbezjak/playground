package magnet

import scala.concurrent.Future

object Use {
  object stub {
    val status   = StatusCodes.OK
    val headers  = List(HttpHeaders.CONTENT_PLAIN)
    val body     = "a response body"
    val response = HttpResponse(status, headers, body)
  }

  def problematic: Unit = {
    Problematic.complete(stub.status)
    Problematic.complete(stub.body)
    Problematic.complete(stub.status, stub.body)
    Problematic.complete(stub.status, stub.headers, stub.body)
    Problematic.complete(stub.response)
    Problematic.complete(Future.successful(stub.response))
    ProblematicMore.complete(Future.successful(stub.status))
  }

  def magnetic: Unit = {
    Magnetic.complete(stub.status, stub.body)
    Magnetic.complete(Future.successful(stub.response))

    import CompletionMagnetMoreImplicits._
    Magnetic.complete(Future.successful(stub.status))
  }

  def all: Unit = {
    println("Use code known as problematic:")
    problematic

    println("\n\nUse code known as magnetic:")
    magnetic
  }
}
