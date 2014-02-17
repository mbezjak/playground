package json

import groovy.json.JsonBuilder
import groovy.json.JsonOutput
import groovy.json.JsonSlurper

class Main {

    static void main(String[] args) {
        def instance = new Custom(
            id    : 42,
            name  : 'Simple value object',
            props : [a:1, b:2],
            types : [Type.A, Type.C],
            when  : new Date()
        )
        println "Instance: " + instance

        def json = JsonOutput.toJson(instance)
        println "As json: " + JsonOutput.prettyPrint(json)

        def map = new JsonSlurper().parseText(json)
        println "As map: " + map

        def back = new Custom(
            id    : map.id,
            name  : map.name,
            props : map.props,
            types : map.types.collect { Type.valueOf(it) },
            when  : Date.parse("yyyy-MM-dd'T'HH:mm:ssZ", map.when)
        )
        println "As object: " + back



        def root = new JsonBuilder().call {
            id    42
            name  'Simple value object'
            props a:1, b:2
            types Type.A, Type.C
            when  new Date()
        }
        println "Built: " + root.toString()
    }

    @groovy.transform.Immutable
    static class Custom {
        int    id
        String name
        Map    props
        List   types
        Date   when
    }

    static enum Type { A, B, C }

}
