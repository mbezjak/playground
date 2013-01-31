import java.security.{AccessController, PrivilegedAction}

object ScalaSecurityImplicits {
  implicit def functionToPrivilegedAction[A](func: Function0[A]) =
    new PrivilegedAction[A] {
      override def run() = func()
    }
}

def testSecurity() {
  import ScalaSecurityImplicits._
  AccessController.doPrivileged( () => println("This is privileged") )
}


object file {
  import java.io.File

  class FileWrapper(val file: File) {
    def / (next: String): FileWrapper = new FileWrapper(new File(file, next))
    override def toString = file.getCanonicalPath
  }

  object FileWrapper {
    implicit def wrap(file: File): FileWrapper = new FileWrapper(file)
    implicit def unwrap(wrapper: FileWrapper): File = wrapper.file
  }

  def useFile(file: File) = println(file.getCanonicalPath)
}

def testFile() {
  import file.FileWrapper.wrap
  import file.useFile
  var root = new java.io.File("/")
  useFile(root / "tmp")
}
