package transform

import java.util.concurrent.TimeoutException
import java.util.concurrent.TimeUnit

class Timed {

    static void run() {
        reportLong()
        reportShort()
    }

    static reportLong() {
        def c = new Computation()
        try {
            c.work(900)
            c.work(900)
            c.work(900)
        } catch (TimeoutException e) {
            println "@TimedInterrupt: computation.work() took too long and was interrupted"
        }
    }

    static reportShort() {
        new Computation().work(100)
        println "@TimedInterrupt: computation.work() did not take too long"
    }

    @groovy.transform.TimedInterrupt(value = 1L, unit = TimeUnit.SECONDS)
    static class Computation {
        void work(int duration) {
            Thread.sleep(duration)
        }
    }

}
