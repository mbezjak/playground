import static ratpack.groovy.Groovy.groovyTemplate
import static ratpack.groovy.Groovy.ratpack
import ratpack.form.Form

import book.BooksModule
import book.Repository

ratpack {
    modules {
        register new BooksModule()
    }

    handlers {
        get('ping') { render([pong: true]) }

        get('book/names') { Repository repository ->
            def names = repository.names()

            render([names: names])
        }

        get('book/:name') { Repository repository ->
            def name = pathTokens.name
            def book = repository[name] ?: [:]

            render book
        }

        post('book') { Repository repository ->
            def book = parse(Form)
            def name = book.name ?: 'unnamed'
            repository.put(name, book)

            def created = 201
            response.status(created)
            response.send ''
        }
    }
}
