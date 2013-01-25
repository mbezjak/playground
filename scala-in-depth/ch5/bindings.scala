package test

object Test {
  def main(args: Array[String]): Unit = {
    testSamePackage()
    testWildcardImport()
    testExplicitImport()
    testInlineDefinition()
  }

  def testSamePackage() { println(x) }

  def testWildcardImport() {
    import Wildcard._
    println(x)
  }

  def testExplicitImport() {
    import Explicit.x
    import Wildcard._
    println(x)
  }

  def testInlineDefinition() {
    val x = "Inline definition x"
    import Explicit.x
    import Wildcard._
    println(x)
  }
}

object Wildcard {
  def x = "Wildcard import x"
}

object Explicit {
  def x = "Explicit import x"
}
