package com.twitter.searchbird

import com.twitter.conversions.time._
import com.twitter.logging.Logger
import com.twitter.util._
import java.util.concurrent.Executors
import scala.collection.mutable
import config._

class SearchbirdServiceImpl(config: SearchbirdServiceConfig, index: Index) extends SearchbirdService.ThriftServer {
  val serverName = "Searchbird"
  val thriftPort = config.thriftPort
  override val tracerFactory = config.tracerFactory

  /**
   * These services are based on finagle, which implements a nonblocking server.  If you
   * are making blocking rpc calls, it's really important that you run these actions in
   * a thread pool, so that you don't block the main event loop.  This thread pool is only
   * needed for these blocking actions.  The code looks like:
   *
   *     val futurePool = new FuturePool(Executors.newFixedThreadPool(config.threadPoolSize))
   *
   *     def hello() = futurePool {
   *       someService.blockingRpcCall
   *     }
   *
   */

  def get(key: String) = index.get(key)
  def put(key: String, value: String) =
    index.put(key, value) flatMap { _ => Future.Unit }
  def search(query: String) = index.search(query)

  def shutdown() = {
    super.shutdown(0.seconds)
  }
}
