import org.jboss.netty.handler.codec.http.{DefaultHttpRequest, HttpRequest, HttpResponse, HttpVersion, HttpMethod}
import com.twitter.finagle.Service
import com.twitter.finagle.builder.ClientBuilder
import com.twitter.finagle.http.Http

object MyClient {
  def connect(path: String) {
    val client: Service[HttpRequest, HttpResponse] = ClientBuilder()
      .codec(Http())
      .hosts("localhost:10000") // start server first
      .hostConnectionLimit(1)
      .build()

    val req = new DefaultHttpRequest(HttpVersion.HTTP_1_1, HttpMethod.GET, path)

    val f = client(req) // send request

    // handle response
    f onSuccess { res =>
      println("got response", res)
    } onFailure { exc =>
      println("failed with", exc)
    }
  }
}
