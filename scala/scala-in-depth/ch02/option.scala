import java.io.File

def getTemporaryDirectory(dirname: Option[String]): File = {
  dirname.map(name => new File(name)).
    filter(_.isDirectory).
    getOrElse(new File(System.getProperty("java.io.tmpdir")))
}


import java.sql.{Connection, DriverManager}
def createConnection(conn_url:  Option[String],
                     conn_user: Option[String],
                     conn_pw:   Option[String]): Option[Connection] =
  for {
    url  <- conn_url
    user <- conn_user
    pw   <- conn_pw
  } yield DriverManager.getConnection(url, user, pw)

def lift3[A,B,C,D](f: Function3[A,B,C,D]): Function3[Option[A], Option[B],
                                                     Option[C], Option[D]] =
  (oa: Option[A], ob: Option[B], oc: Option[C]) =>
    for (a <- oa; b <- ob; c <- oc) yield f(a, b, c)

val createConnection2 = lift3(DriverManager.getConnection)
