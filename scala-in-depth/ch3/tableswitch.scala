import scala.annotation.switch

// javap -v holder$
object holder {
  def foo(x: Int) = (x: @switch) match {
    case 1 => "One!"
    case 2 => "Two!"
    case x => x + "?"
  }
}
