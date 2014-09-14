def sayHelloVia(Closure backend) { backend('hello') }

class Accumulation {
    def accumulated = []
    def add(String s) {
        accumulated += s
    }
}
def accumulation = new Accumulation()

sayHelloVia accumulation.&add
sayHelloVia accumulation.&add
println "Accumulated: $accumulation.accumulated"

sayHelloVia { println it }
sayHelloVia System.out.&println
sayHelloVia this.&println
