import spock.genesis.Gen
import spock.lang.*

class ParseSpec extends Specification {

    @Unroll
    def "parse size property for source='#source'"() {
        given:
        def (key, value) = parseImpl(source)

        expect:
        source.size() == key.size() + '='.size() + value.size()

        where:
        source << Gen.using {
            def k = Gen.string.filter({ !it.contains('=') }).next()
            def v = Gen.string.next()
            k + '=' + v
        }.take(100)
    }

    private List<String> parseImpl(String source) {
        def index = source.indexOf('=')
        [source.substring(0, index), source.substring(index+1)]
    }

}
