import java.net.{Socket, ServerSocket}
import java.util.concurrent.{Executors, ExecutorService}
import java.util.Date

object holder {
  class NetworkService(port: Int, poolSize: Int) extends Runnable {
    val serverSocket = new ServerSocket(port)
    val pool: ExecutorService = Executors.newFixedThreadPool(poolSize)

    override def run() {
      try {
        while (true) {
          val socket = serverSocket.accept() // blocks
          pool.execute(new Handler(socket))
        }
      } finally {
        pool.shutdown()
      }
    }
  }

  class Handler(socket: Socket) extends Runnable {
    def message = (Thread.currentThread.getName() + "\n").getBytes

    override def run() {
      socket.getOutputStream.write(message)
      socket.getOutputStream.close()
    }
  }
}

import holder._

new Thread((new NetworkService(2020, 2))).start()
