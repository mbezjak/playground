import com.twitter.util.{Try,Future,Promise}

// a fetchable thing
trait Resource {
  def imageLinks(): Seq[String]
  def links(): Seq[String]
}

// HTML pages can link to Imgs and to ther HTML pages.
class HTMLPage(val i: Seq[String], val l: Seq[String]) extends Resource {
  override def imageLinks() = i
  override def links() = l
}

// IMGs don't actually link to anything else
class Img() extends Resource {
  override def imageLinks() = Seq()
  override def links() = Seq()
}

object Crawler {

  // profile.html links to gallery.html and has an image link to portrait.jpg
  val profile  = new HTMLPage(Seq("portrait.jpg"), Seq("gallery.html"))
  val portrait = new Img

  // gallery.html links to profile.html and two images
  val gallery = new HTMLPage(Seq("kitten.jpg", "puppy.jpg"), Seq("profile.html"))
  val kitten  = new Img
  val puppy   = new Img

  val internet: Map[String, Resource] = Map(
    "profile.html" -> profile,
    "gallery.html" -> gallery,
    "portrait.jpg" -> portrait,
    "kitten.jpg"   -> kitten,
    "puppy.jpg"    -> puppy
  )

  // fetch(url) attempts to fetch a resource from our fake internet.
  // Its returned Future might contain a Resource of an exception
  def fetch(url: String): Future[Resource] = new Promise(Try(internet(url)))

  def getThumbnail(url: String): Future[Seq[Resource]] =
    fetch(url) flatMap { page =>
      Future.collect(page.imageLinks map fetch)
    }

  def crawl(url: String): Future[Seq[Resource]] =
    fetch(url) flatMap { page =>
      Future.collect(page.links map crawl) map { pps => pps.flatten }
    }
}
