package camel

import org.apache.camel.impl.DefaultCamelContext
import org.apache.camel.builder.RouteBuilder

class Main {

    static void main(String[] args) {
        def context = new DefaultCamelContext()

        context.addRoutes(new RouteBuilder() {
            void configure() {
                from('file:///tmp/in').to('file:///tmp/out')
            }
        })

        def template = context.createProducerTemplate()

        context.start()


        10.times { n ->
            template.sendBody('file:///tmp/in', "Test message: $n")
        }
        template.sendBodyAndHeader('file:///tmp/in', new java.io.File('build.gradle'), "CamelFileName", 'build.gradle')

        Thread.sleep 60 * 1000
    }

}
