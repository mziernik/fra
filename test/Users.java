
import com.context.unit_test.GroupTest;
import users.Groups;

import org.junit.runner.RunWith;
import org.junit.runners.AllTests;

@RunWith(AllTests.class)
public class Users extends GroupTest {

    static {
        add(users.Users.class);
        add(Groups.class);
    }

}
