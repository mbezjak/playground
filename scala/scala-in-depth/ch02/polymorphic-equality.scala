trait InstantaneousTime extends Equals {
  val repr: Int

  override def hashCode(): Int = repr.##
  override def canEqual(other: Any) = other.isInstanceOf[InstantaneousTime]
  override def equals(other: Any): Boolean = other match {
    case that: InstantaneousTime =>
      if (this eq that) {
        true
      } else {
        (that.## == this.##) &&
        (that canEqual this) &&
        (repr == that.repr)
      }
    case _ => false
  }
}

trait Event extends InstantaneousTime {
  val name: String

  override def hashCode(): Int = repr.## + (31 * name.##)
  override def canEqual(other: Any) = other.isInstanceOf[Event]
  override def equals(other: Any): Boolean = other match {
    case that: Event =>
      if (this eq that) {
        true
      } else {
        (that canEqual this) &&
        (repr == that.repr) &&
        (name == that.name)
      }
    case _ => false
  }
}
