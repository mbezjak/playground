@Grab("io.ratpack:ratpack-groovy:0.9.3")
import static ratpack.groovy.Groovy.*

ratpack {
  handlers {
    get {
      render "Hello world!"
    }
  }
}
