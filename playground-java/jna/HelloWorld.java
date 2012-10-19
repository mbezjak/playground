import com.sun.jna.Library;
import com.sun.jna.Native;

public class HelloWorld {
    public interface CTest extends Library {
        public void helloFromC();
    }

    public static void main(String args[]) {
        CTest ctest = (CTest) Native.loadLibrary("ctest", CTest.class);
        ctest.helloFromC();
    }
}
