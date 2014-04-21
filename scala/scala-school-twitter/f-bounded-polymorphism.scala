/*
trait Container extends Ordered[Container]

class MyContainer extends Container {
  // compile error
  override def compare(that: MyContainer): Int = 0
}
*/

trait Container[A <: Container[A]] extends Ordered[A]

class MyContainer extends Container[MyContainer] {
  def compare(that: MyContainer) = 0
}

class YourContainer extends Container[YourContainer] {
  def compare(that: YourContainer) = 0
}

List(new MyContainer, new MyContainer).min
List(new MyContainer, new MyContainer, new YourContainer)
