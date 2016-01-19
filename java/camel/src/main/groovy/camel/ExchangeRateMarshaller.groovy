package camel

import org.apache.camel.Exchange
import org.apache.camel.impl.ServiceSupport
import org.apache.camel.spi.DataFormat

class ExchangeRateMarshaller extends ServiceSupport implements DataFormat {

    @Override
    void marshal(Exchange exchange, Object graph, OutputStream stream) throws Exception {
        throw new UnsupportedOperationException('No impl for ExchangeRate marshalling')
    }

    @Override
    Object unmarshal(Exchange exchange, InputStream stream) throws Exception {
        def text = stream.getText('UTF-8')
        text.readLines().tail().collect { line ->
            def (id, buy, avg, sell) = line.tokenize()
            new ExchangeRate(
                id : id,
                buy : hrStringToBigDecimal(buy),
                avg : hrStringToBigDecimal(avg),
                sell : hrStringToBigDecimal(sell)
            )
        }
    }

    private BigDecimal hrStringToBigDecimal(String d) {
        new BigDecimal(d.replace(',', '.'))
    }

    @Override
    protected void doStart() throws Exception {}

    @Override
    protected void doStop() throws Exception {}

}
