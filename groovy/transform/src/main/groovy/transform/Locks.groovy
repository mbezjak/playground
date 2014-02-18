package transform

class Locks {

    static void run() {
        def a = new Attribute()
        a.put('a', 1)
        a.put('b', 2)

        def report = {
            println "@With{Read,Write}Lock: a.get('a') = ${a.get('a')}"
            println "@With{Read,Write}Lock: a.get('b') = ${a.get('b')}"
            println "@With{Read,Write}Lock: a.get('c') = ${a.get('c')}"
        }

        println "This will not be deterministic"
        Thread.start report
        Thread.start {
            report()
            a.put('c', 3)
            report()
        }
        Thread.start report

        Thread.sleep 100
    }

    static class Attribute {
        private Map map = [:]

        @groovy.transform.WithReadLock
        def get(key) {
            map[key]
        }

        @groovy.transform.WithWriteLock
        def put(key, val) {
            map[key] = val
        }
    }

}
