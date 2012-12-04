package interop;

import scala.runtime.AbstractFunction0;
import scala.runtime.AbstractFunction1;

public class ClosureClassUse {

    public static void main(String[] args) {
        ClosureClass c = new ClosureClass();
        c.printResult(new AbstractFunction0<String>() {
                public String apply() {
                    return "foo";
                }
            });
        c.printResult(new AbstractFunction1<String, String>() {
                public String apply(String arg) {
                    return arg + " foo";
                }
            });
    }

}
