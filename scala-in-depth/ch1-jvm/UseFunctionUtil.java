public class UseFunctionUtil {

    public static void main(String[] args) {
        System.out.println(FunctionUtil.testFunction(
            new scala.runtime.AbstractFunction1<Integer, Integer>() {
                public Integer apply(Integer x) {
                    return x + 5;
                }
            }));
    }

}
