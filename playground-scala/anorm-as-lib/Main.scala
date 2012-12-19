import java.sql._
import anorm._
import anorm.Column._
import anorm.SqlParser._

object Main extends App {
  val driver   = "com.driver.name"
  val url      = "jdbc:url"
  val username = "username"
  val password = "password"

  Class.forName(driver)
  val conn = DriverManager.getConnection(url, username, password)

  def time[T](work: => T) {
    val start  = System.nanoTime()
    val result = work
    val end    = System.nanoTime()

    val t = (end - start) / 1e9
    println("result = " + result)
    println("time = " + t + " s")
  }

  time {
    val sql = SQL("select * from foo")
    sql()(conn).map { row =>
      row[Int]("column")
    }.sum
  }

  conn.close()
}
