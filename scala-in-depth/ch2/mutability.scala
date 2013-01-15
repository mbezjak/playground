class Vector2D(var x: Double, var y: Double) {
  def magnify(amt: Double): Vector2D = {
    x *= amt
    y *= amt
    this
  }

  def - (v: Vector2D): Vector2D = {
    x -= v.x
    y -= v.y
    this
  }

  override def toString = x + ", " + y
}

val x = new Vector2D(1.0,  1.0)
val y = new Vector2D(-1.0, 1.0)
x.magnify(3.0) - (x - y).magnify(3.0)
