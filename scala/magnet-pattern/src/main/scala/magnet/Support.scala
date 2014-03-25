package magnet

case class StatusCode(code: Int)
case class HttpHeader(name: String, value: String)
case class HttpResponse(status: StatusCode, headers: List[HttpHeader], body: String)

object StatusCodes {
  val OK = StatusCode(200)
}
object HttpHeaders {
  val CONTENT_PLAIN = HttpHeader("Content-Type", "text/plain")
}


trait Marshaller[T] {
  def marshal(body: T): HttpResponse
}

object Marshaller {
  implicit def stringM: Marshaller[String] =
    new Marshaller[String] {
      override def marshal(body: String) = HttpResponse(StatusCodes.OK, Nil, body)
    }
}
