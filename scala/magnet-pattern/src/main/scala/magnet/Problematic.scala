package magnet

import scala.concurrent.Future

object Problematic {
  def complete(status: StatusCode): Unit = {
    println(s"Completed with status code = $status")
  }

  def complete[T : Marshaller](obj: T): Unit = {
    println(s"Completed with marshaller = $obj")
  }

  def complete[T : Marshaller](status: StatusCode, obj: T): Unit = {
    println(s"Completed with status code = $status and marshaller = $obj")
  }

  def complete[T : Marshaller](status: StatusCode, headers: List[HttpHeader], obj: T): Unit = {
    println(s"Completed with status code = $status, headers = $headers and marshaller = $obj")
  }

  def complete(response: HttpResponse): Unit = {
    println(s"Completed with response = $response")
  }

  def complete(future: Future[HttpResponse]): Int = {
    println(s"Completed with a future of response = $future")
    42
  }

}

object ProblematicMore {

  // cannot be a part of Problematic due to erasure
  def complete(future: Future[StatusCode]): Int = {
    println(s"Completed with a future of status code = $future")
    24
  }

}
