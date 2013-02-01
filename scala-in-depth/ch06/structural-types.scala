object Resources {
  type Resource = { def close(): Unit }
  def close(r: Resource) = r.close()
}

val bos = new java.io.ByteArrayOutputStream()
val bis = new java.io.ByteArrayInputStream(Array[Byte]())
Resources.close(bos)
Resources.close(bis)


// nesting
type T = {
  type X = Int
  def x: X

  type Y
  def y: Y
}

object Foo {
  type X = Int
  def x: X = 5

  type Y = String
  def y: Y = "Hello world"
}

def nested(t: T): t.X = t.x
