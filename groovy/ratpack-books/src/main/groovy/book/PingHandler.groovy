package book

import ratpack.groovy.handling.GroovyHandler
import ratpack.groovy.handling.GroovyContext

class PingHandler extends GroovyHandler {

    @Override
    void handle(GroovyContext context) {
        context.response.send('pong')
    }

}
