trait Logger {
  def log(category: String, msg: String): Unit = println(msg)
}

trait RemoteLogger extends Logger {
  override def log(category: String, msg: String): Unit = {
    // send over socket
  }
}

trait NullLogger extends Logger {
 override def log(category: String, msg: String): Unit = {}
}

trait HasLogger {
  val logger: Logger = new Logger {}
}

trait HasRemoteLogger extends HasLogger {
  override val logger: Logger = new RemoteLogger {}
}

trait HasNullLogger extends HasLogger {
  override val logger: Logger = new NullLogger {}
}

trait DataAccess extends HasLogger {
  def query(in: String): Int = 42
}

trait LoggedDataAccess extends DataAccess {
  override def query(in: String): Int = {
    logger.log("QUERY", in)
    super.query(in)
  }
}
