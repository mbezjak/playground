package interop

// http://www.scala-lang.org/node/106

import java.io.IOException
import scala.throws
import scala.reflect.{BeanProperty, BooleanBeanProperty}

class SimpleClass(name: String, val acc: String, @BeanProperty var mutable: String) {
  val foo = "foo"
  var bar = "bar"
  @BeanProperty val fooBean = "foobean"
  @BeanProperty var barBean = "barbean"
  @BooleanBeanProperty var awesome = true

  def dangerFoo() = {
    throw new IOException("SURPRISE!")
  }

  @throws(classOf[IOException])
  def dangerBar() = {
    throw new IOException("NO SURPRISE!")
  }
}
