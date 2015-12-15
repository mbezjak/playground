package camel

import org.apache.camel.impl.DefaultCamelContext
import org.apache.camel.builder.RouteBuilder

class Main {

    static void main(String[] args) {
        def context = new DefaultCamelContext()

        context.addRoutes(new RouteBuilder() {
            void configure() {
                from('file:///tmp/in').to('file:///tmp/out')
                from('file:///tmp/out').to('log://camel.SyncInToOut?showHeaders=true')
                from('timer:hnb?period=1000')
                    .to('http4://hnb.hr/tecajn/f151215.dat')
                    .marshal().string('UTF-8')
                    .to('log://camel.Hnb')
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
