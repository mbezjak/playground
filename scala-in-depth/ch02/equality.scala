class Point2(var x: Int, var y: Int) extends Equals {
  def move(mx: Int, my: Int): Unit = {
    x = x + mx
    y = y + my
  }

  override def hashCode(): Int = y + (31*x)
  override def canEqual(that: Any): Boolean = that match {
    case p: Point2 => true
    case _ => false
  }
  override def equals(that: Any): Boolean = {
    def strictEquals(other: Point2) =
      this.x == other.x && this.y == other.y

    that match {
      case a: AnyRef if this eq a => true
      case p: Point2 => (p canEqual this) && strictEquals(p)
      case _ => false
    }
  }

  override def toString: String = x + ", " + y
}
