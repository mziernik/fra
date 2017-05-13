package users;

import com.user.right.RCache;
import com.user.UserType;
import com.user.BaseUsersHandler;
import com.user.right.RUsers;
import com.user.right.RSAdmin;
import com.user.RightsScheme;
import com.user.UserStatus;
import com.user.BaseUserData;
import com.user.right.UserRight;
import com.user.right.RCron;
import com.utils.Utils;
import com.servlet.Handlers;
import context.ServiceTest;
import java.util.LinkedList;
import org.junit.*;
import users.Groups.GAdmins;
import users.Groups.GAgents;

/**
 * @author Miłosz Ziernik
 * @date 17 grudnia 2015
 * @encoding UTF-8
 */
public class Users extends ServiceTest {

    static {
        require(Groups.class);
    }

    @Test()
    public void t01_register() throws Exception {

        BaseUserData user = new BaseUserData(null);
        user.status = UserStatus.active;
        user.type = UserType.normal;

        user.username = "login";
        user.passPlain = "haslo";
        user.createdBy = "Kreator";
        user.email = "adres@email";
        user.ldapAuth = false;
        user.token = "unikalny_token";
        user.firstname = "Imię";
        user.lastname = "Nazwisko";
        user.displayName = "Użytkownik";

        user.rights.allow(RSAdmin.class);
        user.rights.allow(RUsers.class);

        user.rights.denny(RCache.class);
        user.rights.denny(RCron.class);

        user.rights.addGroup(RightsScheme.get(GAdmins.class));
        user.rights.addGroup(RightsScheme.get(GAgents.class));

        Handlers.users.getInstance().editUser(user, true);
        assertTrue(user.id != null);

        //-------------------------------------------------------------
        user = Handlers.users.getInstance().getUser("login");
        assertEquals(UserStatus.active, user.status);
        assertEquals("login", user.username);
        assertEquals("207023CCB44FEB4D7DADCA005CE29A64", user.passHash);
        assertEquals("Kreator", user.createdBy);
        assertEquals("adres@email", user.email);
        assertEquals("unikalny_token", user.token);
        assertEquals(false, user.ldapAuth);
        assertEquals("Imię", user.firstname);
        assertEquals("Nazwisko", user.lastname);
        assertEquals("Użytkownik", user.displayName);

        LinkedList<UserRight> allowed = Utils.asList(user.rights.allowed);
        assertEquals(UserRight.get(RSAdmin.class), allowed.poll());
        assertEquals(UserRight.get(RUsers.class), allowed.poll());
        Assert.assertTrue(allowed.isEmpty());

        LinkedList<UserRight> denied = Utils.asList(user.rights.denied);
        assertEquals(UserRight.get(RCache.class), denied.poll());
        assertEquals(UserRight.get(RCron.class), denied.poll());
        Assert.assertTrue(denied.isEmpty());

        LinkedList<RightsScheme> gg = Utils.asList(user.rights.groups);
        assertEquals(GAdmins.class, gg.poll().getClass());
        assertEquals(GAgents.class, gg.poll().getClass());
    }

    @Test()
    public void t02_edit() throws Exception {
        BaseUsersHandler handler = Handlers.users.getInstance();
        BaseUserData user = handler.getUser("login");
        assertTrue(user != null);

        user.status = UserStatus.disabled;
        user.type = UserType.virtual;
        user.username = "nowyLogin";
        user.passHash = "ppp";
        user.createdBy = "ROOT";
        user.email = "dfsglkdfjsgk@dsfsd.ss";
        user.token = "{238472305808934}";
        user.ldapAuth = true;
        user.firstname = "FIRSTNAME";
        user.lastname = "LASTNAME";
        user.displayName = "qwerty";

        user.rights.allowed.remove(UserRight.get(RSAdmin.class));
        user.rights.denied.remove(UserRight.get(RCache.class));
        user.rights.groups.remove(RightsScheme.get(GAdmins.class));

        handler.editUser(user, false);
        //------------------------------------------------------------------
        user = handler.getUser("nowyLogin");
        assertTrue(user != null);

        assertEquals(UserStatus.disabled, user.status);
        assertEquals(UserType.virtual, user.type);
        assertEquals("nowyLogin", user.username);
        assertEquals("ppp", user.passHash);
        assertEquals("ROOT", user.createdBy);
        assertEquals("dfsglkdfjsgk@dsfsd.ss", user.email);
        assertEquals("{238472305808934}", user.token);
        assertEquals(false, user.ldapAuth);
        assertEquals("FIRSTNAME", user.firstname);
        assertEquals("LASTNAME", user.lastname);
        assertEquals("qwerty", user.displayName);

        LinkedList<UserRight> allowed = Utils.asList(user.rights.allowed);
        assertEquals(UserRight.get(RUsers.class), allowed.poll());
        Assert.assertTrue(allowed.isEmpty());

        LinkedList<UserRight> denied = Utils.asList(user.rights.denied);
        assertEquals(UserRight.get(RCron.class), denied.poll());
        Assert.assertTrue(denied.isEmpty());

        LinkedList<RightsScheme> gg = Utils.asList(user.rights.groups);
        assertEquals(GAgents.class, gg.poll().getClass());
    }

    @Test(expected = Error.class)
    public void t03_registerRootErr() throws Exception {
        BaseUserData user = new BaseUserData(null);
        user.username = "root";
        user.displayName = "ROOT";
        user.passPlain = "1234";
        Handlers.users.getInstance().editUser(user, true);
    }

    @Test(expected = UnsupportedOperationException.class)
    public void t04_removeRoot() throws Exception {
        Handlers.users.getInstance().removeUser(
                Handlers.users.getInstance().getUser("root")
        );
    }

    @Test()
    public void t05_remove() throws Exception {

        BaseUsersHandler handler = Handlers.users.getInstance();

        BaseUserData user = handler.getUser("nowyLogin");
        Assert.assertTrue(user != null && user.username.equals("nowyLogin"));

        handler.removeUser(user);

        user = handler.getUser("login");
        Assert.assertTrue(user == null);

        user = handler.getUser("nowyLogin");
        Assert.assertTrue(user == null);

    }
}
