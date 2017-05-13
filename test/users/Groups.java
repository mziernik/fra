package users;

import com.user.right.RConfig;
import com.user.BaseUsersHandler;
import com.user.right.RSAdmin;
import com.user.RightsScheme;
import com.user.right.UserRight;
import com.user.right.RCron;
import com.user.right.RStatus;
import com.context.index.Index;
import com.servlet.Handlers;
import context.ServiceTest;
import org.junit.*;
import org.junit.runners.MethodSorters;
import users.Rights.RRight1;

/**
 * @author Miłosz Ziernik
 * @date 17 grudnia 2015
 * @encoding UTF-8
 */
@FixMethodOrder(MethodSorters.NAME_ASCENDING)
public class Groups extends ServiceTest {

    static {
        require(Rights.class);
        Index.groups.add(GAdmins.class);
        Index.groups.add(GAgents.class);
    }

    @Test()
    public void test1register() throws Exception {

        RightsScheme group = new RightsScheme("grupa1", "Grupa 1", RStatus.class);
        group.details = "Szczegóły grupy 1";
        group.enabled = true;
        group.allow(RRight1.class);
        group.refuse(RSAdmin.class);

        BaseUsersHandler handler = Handlers.users.getInstance();
        handler.editGroup(group, true);

        handler.reload();

        //----------------------------------------------------------------------
        group = RightsScheme.get("grupa1", true);
        assertEquals("Grupa 1", group.name);
        assertEquals(true, group.enabled);
        assertEquals(false, group.embedded);
        assertEquals("Szczegóły grupy 1", group.details);

        assertTrue(group.allowed.contains(UserRight.get(RStatus.class)));
        assertTrue(group.allowed.contains(UserRight.get(RRight1.class)));
        assertTrue(group.refused.contains(UserRight.get(RSAdmin.class)));

    }

    @Test()
    public void test2edit() throws Exception {

        RightsScheme group = RightsScheme.get("grupa1", true);
        Assert.assertTrue(group != null);

        group.name = "Nowa nazwa";
        group.details = "Nowy opis";
        group.enabled = false;

        group.allow(RCron.class);
        group.refuse(RConfig.class);

        BaseUsersHandler handler = Handlers.users.getInstance();
        handler.editGroup(group, false);

        handler.reload();

        //----------------------------------------------------------------------
        group = RightsScheme.get("grupa1", true);
        assertEquals("Nowa nazwa", group.name);
        assertEquals(false, group.enabled);
        assertEquals(false, group.embedded);
        assertEquals("Nowy opis", group.details);

        assertTrue(group.allowed.contains(UserRight.get(RStatus.class)));
        assertTrue(group.allowed.contains(UserRight.get(RCron.class)));
        assertTrue(group.allowed.contains(UserRight.get(RRight1.class)));
        assertTrue(group.refused.contains(UserRight.get(RSAdmin.class)));
        assertTrue(group.refused.contains(UserRight.get(RConfig.class)));

    }

    @Test()
    public void test3remove() throws Exception {

        RightsScheme group = RightsScheme.get("grupa1", true);
        Assert.assertTrue(group != null);

        BaseUsersHandler handler = Handlers.users.getInstance();
        handler.removeGroup(group);
        handler.reload();

        group = RightsScheme.get("grupa1", false);
        Assert.assertTrue(group == null);

    }

    public static class GAdmins extends RightsScheme {

        public GAdmins() {
            super("admins", "Administratorzy", RSAdmin.class);
        }

    }

    public static class GAgents extends RightsScheme {

        public GAgents() {
            super("agents", "Agenci");
        }

    }

}
