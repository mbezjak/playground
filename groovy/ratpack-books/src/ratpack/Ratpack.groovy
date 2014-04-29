import static ratpack.groovy.Groovy.groovyTemplate
import static ratpack.groovy.Groovy.ratpack

import book.PingHandler

ratpack {
    handlers {
        get('ping', new PingHandler())

        get {
            render groovyTemplate("index.html", title: "My Ratpack App")
        }

        assets "public"
    }
}
