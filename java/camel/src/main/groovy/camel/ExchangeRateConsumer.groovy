package camel

class ExchangeRateConsumer {

    String consume(String dat) {
        def eur = dat.readLines().find { it.contains('EUR') }
        def avgRate = eur.tokenize()[2]
        avgRate
    }

}
