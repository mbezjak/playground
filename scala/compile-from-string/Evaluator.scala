// source: https://groups.google.com/d/msg/scala-user/kQseoMNQSPw/foO4ar0Gs5QJ
object Evaluator {
  import scala.reflect.runtime._
  import scala.tools.reflect.ToolBox

  def eval[T](code: String, imports: Seq[String] = Seq()): T = {
    val cm = universe.runtimeMirror(getClass.getClassLoader)
    val tb = cm.mkToolBox()
    val script = imports.map("import " + _).mkString("\n") + "\n{\n" + code + "\n}"

    tb.eval(tb.parse(script)).asInstanceOf[T]
  }
}
