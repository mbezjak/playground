package book

import ratpack.groovy.handling.GroovyContext
import ratpack.groovy.render.GroovyRendererSupport

import groovy.json.JsonOutput

class JsonRenderer extends GroovyRendererSupport<Map> {

    @Override
    void render(GroovyContext context, Map result) {
        context.byContent {
            json {
                render JsonOutput.toJson(result)
            }
        }
    }

}
