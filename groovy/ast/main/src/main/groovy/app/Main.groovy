package app

class Main {

    static void main(String[] args) {
        def properties = new Properties()
        Runner.run(Use.dsl, properties)

        println "Properties: ${properties.props}"
    }

}
