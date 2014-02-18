package transform

class Memo {

    static void run() {
        def c = new Computation()
        report c
        report c
    }

    static void report(Computation c) {
        def start = System.currentTimeMillis()
        c.work(42)
        def end = System.currentTimeMillis()

        println "@Memoized: computation.work() took ${end-start} ms"
    }

    static class Computation {
        @groovy.transform.Memoized
        int work(int arg) {
            Thread.sleep 2000
            arg
        }
    }

}
