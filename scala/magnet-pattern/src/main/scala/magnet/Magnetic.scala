package magnet

import scala.language.implicitConversions
import scala.concurrent.Future

sealed trait CompletionMagnet {
  type Result
  def apply(): Result
}

object CompletionMagnet {
  implicit def fromStatusObject[T : Marshaller](tuple: (StatusCode, T)): CompletionMagnet =
    new CompletionMagnet {
      type Result = Unit
      override def apply(): Result = println(toString)

      override def toString = s"Magnet from status code = ${tuple._1} and body = ${tuple._2}"
    }

  implicit def fromHttpResponseFuture(future: Future[HttpResponse]): CompletionMagnet =
    new CompletionMagnet {
      type Result = Int
      override def apply(): Result = {
        println(toString)
        42
      }

      override def toString = s"Magnet from future of response = $future"
    }
}

// should actually be defined in StatusCode companion object
object CompletionMagnetMoreImplicits {
  implicit def fromHttpResponseFuture(future: Future[StatusCode]): CompletionMagnet =
    new CompletionMagnet {
      type Result = Int
      override def apply(): Result = {
        println(toString)
        24
      }

      override def toString = s"Magnet from future of status code = $future"
    }
}

object Magnetic {
  def complete(magnet: CompletionMagnet): magnet.Result = magnet()
}
