
import com.context.unit_test.GroupTest;

import org.junit.runner.RunWith;
import org.junit.runners.AllTests;
import service.Auth;

@RunWith(AllTests.class)
public class Controllers extends GroupTest {

    static {
        add(service.Controllers.class);
        add(Auth.class);
    }

}
