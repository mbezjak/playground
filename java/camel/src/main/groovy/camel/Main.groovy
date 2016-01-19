package camel

import org.apache.camel.LoggingLevel
import org.apache.camel.impl.DefaultCamelContext
import org.apache.camel.impl.SimpleRegistry
import org.apache.camel.builder.RouteBuilder

class Main {

    static void main(String[] args) {
        def context = new DefaultCamelContext()

        def sshUrl = 'root:password@host'

        context.addRoutes(new RouteBuilder() {
            void configure() {
                from('file:///tmp/in').to('file:///tmp/out')
                from('file:///tmp/out').to('log:camel.log.SyncInToOut?showHeaders=true')

                from('timer:hnb?period=5000')
                    .to('http4://hnb.hr/tecajn/f151215.dat')
                    .unmarshal().string('UTF-8')
                    .to('log:camel.log.HnbExchangeRateFile')
                    .to('bean:exchangeRateConsumer?method=consume')
                    .log(LoggingLevel.INFO, 'camel.log.HnbEurAvgRate', 'EUR avg exchange rate: ${body}')

                from('timer:hnb?period=5000')
                    .to('http4://hnb.hr/tecajn/f151215.dat')
                    .unmarshal().custom('exchangeRateMarshaller')
                    .to('bean:exchangeRateConsumer?method=consume')
                    .log(LoggingLevel.INFO, 'camel.log.HnbEurAvgRateFromClass', 'EUR avg exchange rate from class: ${body}')

                from('file:///tmp/print').to('lpr://localhost/default?mediaSize=ISO_A4')
                from('direct:ssh').to("ssh:$sshUrl").log(LoggingLevel.INFO, 'camel.log.Ssh', 'SSH exec: ${body}')
            }
        })

        context.registry = {
            def registry = new SimpleRegistry()
            registry.exchangeRateConsumer = new ExchangeRateConsumer()
            registry.exchangeRateMarshaller = new ExchangeRateMarshaller()
            registry
        }()

        def template = context.createProducerTemplate()

        context.start()


        10.times { n ->
            template.sendBody('file:///tmp/in', "Test message: $n")
        }
        template.sendBodyAndHeader('file:///tmp/in', new java.io.File('build.gradle'), "CamelFileName", 'build.gradle')

        //template.sendBody('file:///tmp/print/fox', 'The quick brown fox jumps over the lazy dog')
        template.sendBody('direct:ssh', 'ls -al /')

        Thread.sleep 60 * 1000
    }

}
