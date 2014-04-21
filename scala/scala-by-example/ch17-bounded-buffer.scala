class BoundedBuffer[A : ClassManifest](N: Int) {
  var in  = 0
  var out = 0
  var n   = 0
  val elems = Array.ofDim[A](N)

  def put(x: A) = synchronized {
    while (n >= N) wait()

    elems(in) = x
    in   = (in + 1) % N
    n     = n + 1

    if (n == 1) notifyAll()
  }

  def get: A = synchronized {
    while (n == 0) wait()

    val x = elems(out)
    out   = (out + 1) % N
    n     = n - 1

    if (n == N - 1) notifyAll()
    x
  }

}


import concurrent.ops._
val buf = new BoundedBuffer[String](10)
spawn { for (_ <- 1 to 10) { buf put "hello"  } }
spawn { for (_ <- 1 to 10) { println(buf.get) } }
