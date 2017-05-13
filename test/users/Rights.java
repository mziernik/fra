package users;

import com.context.index.Index;
import com.lang.core.LStr;
import com.user.BaseUserData;
import com.user.RightsScheme;
import com.user.UserStatus;
import com.user.UserType;
import com.user.right.RSAdmin;
import com.user.right.RCache;
import com.user.right.RConfig;
import com.user.right.RCron;
import com.user.right.RRights;
import com.user.right.RRoot;
import com.user.right.RServiceDb;
import com.user.right.RStatus;
import com.user.right.RTest;
import com.user.right.RUsers;
import com.user.right.UserRight;
import context.ServiceTest;
import org.junit.*;
import org.junit.runners.MethodSorters;

/**
 * @author Miłosz Ziernik
 * @date 17 grudnia 2015
 * @encoding UTF-8
 */
@FixMethodOrder(MethodSorters.NAME_ASCENDING)
public class Rights extends ServiceTest {

    static {
        Index.rights.add(RRight1.class);
    }

    public BaseUserData getUser() {
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

        return user;
    }

    public void clearRights(BaseUserData user) {
        user.rights.allowed.clear();
        user.rights.denied.clear();
        user.rights.groups.clear();
    }

    //################### UPRAWNIENIA ###################
    //    - ROOT (RRoot)
    //      | - Administator (RAdmin)
    //          | - Konfiguracja usługi (RConfig)
    //          | - Zarządzanie harmonogramem zadań (RCron)
    //          | - Podgląd statusu usługi (RStatus)
    //          | - Zarządzanie użytkownikami (RUser)
    //      | - Zarządzanie cache-m (RCache)
    //      | - Zarządzanie uprawnieniami (RRights)
    //      | - Zarządzanie bazą serwisową (RServiceDb)
    //      | - Testy (RTest)
    //###################### GRUPY ######################
    //    - Administratorzy -> GAdmins (allowed: RAdmin)
    //    - Testerzy -> GTesters (allowed: RTest)
    //    - Urzytkownicy -> GUsers (allowed: RAdmin | refused: RCron, RUsers, RConfig)
    //###################################################
    @Test()
    public void test01permissions() throws Exception {
        BaseUserData user = getUser();

        //@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
        //@                         ROLE                             @
        //@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
        // ####################### OPIS #######################
        //  - RRoot
        //    | - RAdmin  (right: denny)
        //        | - RConfig
        user.rights.denny(RSAdmin.class);

        assertFalse(user.rights.has(RRoot.class));
        assertFalse(user.rights.has(RConfig.class));

        // ####################### OPIS #######################
        //  - RRoot (right: denny)
        //    | - RAdmin (right: allow)
        //        | - RConfig
        clearRights(user);
        user.rights.denny(RRoot.class);
        user.rights.allow(RSAdmin.class);

        assertFalse(user.rights.has(RRoot.class));
        assertTrue(user.rights.has(RSAdmin.class));
        assertTrue(user.rights.has(RConfig.class));

        // ####################### OPIS #######################
        //  - RRoot (right: denny)
        //    | - RAdmin (right: allow)
        //        | - RConfig (right: denny)
        clearRights(user);
        user.rights.denny(RRoot.class);
        user.rights.allow(RSAdmin.class);
        user.rights.denny(RConfig.class);

        assertFalse(user.rights.has(RRoot.class));
        assertTrue(user.rights.has(RSAdmin.class));
        assertFalse(user.rights.has(RConfig.class));

        // ####################### OPIS #######################
        //  - RRoot
        //    | - RAdmin (right: denny)
        //        | - RConfig
        //    | - RCache (right: allow)
        clearRights(user);
        user.rights.denny(RSAdmin.class);
        user.rights.allow(RCache.class);

        assertTrue(user.rights.has(RCache.class));
        assertFalse(user.rights.has(RConfig.class));
        assertTrue(user.rights.has(RConfig.class, RCache.class));

        // ####################### OPIS #######################
        //  - RRoot
        //    | - RAdmin (right: allow)
        //        | - RConfig (right: denny)
        //        | - RCron
        //        | - RStatus (right: denny)
        //        | - RUser
        //    | - RCache
        //    | - RRights (right: allow)
        //    | - RServiceDb (right: allow)
        //    | - RTest (right: denny)
        clearRights(user);
        user.rights.allow(RSAdmin.class);
        user.rights.allow(RRights.class);
        user.rights.allow(RServiceDb.class);
        user.rights.denny(RConfig.class);
        user.rights.denny(RStatus.class);
        user.rights.denny(RTest.class);

        assertTrue(user.rights.has(RSAdmin.class, RTest.class, RRoot.class));
        assertFalse(user.rights.has(RRoot.class, RConfig.class, RTest.class, RCache.class));

        //@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
        //@                     ROLE + GRUPY                         @
        //@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
        RightsScheme gAdmins = new GAdmins();
        RightsScheme gTesters = new GTesters();
        RightsScheme gUsers = new GUsers();

        // ####################### OPIS #######################
        //  - RRoot
        //    | - RAdmin (right: denny | group: allow)
        //        | - RConfig
        clearRights(user);
        user.rights.addGroup(gAdmins);
        user.rights.denny(RSAdmin.class);

        assertFalse(user.rights.has(RSAdmin.class, RConfig.class));

        // ####################### OPIS #######################
        //  - RRoot
        //    | - RAdmin (right: denny | group: allow)
        //        | - RConfig
        //    | - RTest (group: allow)
        clearRights(user);
        user.rights.addGroup(gAdmins);
        user.rights.addGroup(gTesters);
        user.rights.denny(RSAdmin.class);

        assertTrue(user.rights.has(RTest.class, RSAdmin.class, RConfig.class));

        // ####################### OPIS #######################
        //  - RRoot
        //    | - RAdmin (group: allow)
        //        | - RConfig
        clearRights(user);
        user.rights.addGroup(gAdmins);

        assertTrue(user.rights.has(RConfig.class));
        assertTrue(user.rights.has(RSAdmin.class));
        assertTrue(user.rights.has(RConfig.class, RSAdmin.class));
        assertFalse(user.rights.has(RRoot.class));

        // ####################### OPIS #######################
        //  - RRoot
        //    | - RAdmin (group: allow)
        //        | - RConfig (role: denny)
        clearRights(user);
        user.rights.addGroup(gAdmins);
        user.rights.denny(RConfig.class);

        assertTrue(user.rights.has(RSAdmin.class));
        assertFalse(user.rights.has(RRoot.class, RConfig.class));

        // ####################### OPIS #######################
        //  - RRoot
        //    | - RAdmin (group: allow)
        //        | - RConfig (group: denny)
        clearRights(user);
        user.rights.addGroup(gUsers);

        assertTrue(user.rights.has(RSAdmin.class));
        assertTrue(user.rights.has(RSAdmin.class, RConfig.class));
        assertFalse(user.rights.has(RRoot.class, RConfig.class));
        assertFalse(user.rights.has(RConfig.class));

        // ####################### OPIS #######################
        //  - RRoot
        //    | - RAdmin (group: allow)
        //        | - RConfig (group: denny)
        clearRights(user);
        user.rights.addGroup(gUsers);

        assertTrue(user.rights.has(RSAdmin.class));
        assertFalse(user.rights.has(RRoot.class, RConfig.class));

        // ####################### OPIS #######################
        //  - RRoot (role: allow)
        //    | - RAdmin (group: allow)
        //        | - RConfig (group: denny)
        clearRights(user);
        user.rights.addGroup(gUsers);
        user.rights.allow(RRoot.class);

        assertTrue(user.rights.has(RSAdmin.class, RRoot.class, RConfig.class));
    }

    @Test()
    public void test2edit() throws Exception {

    }

    @Test()
    public void test3remove() throws Exception {

    }

    public static class RRight1 extends UserRight {

        public RRight1() {
            super("right1", new LStr("Uprawnienie 1"));
        }
    }

    private class GAdmins extends RightsScheme {

        public GAdmins() {
            super("gadmins", "Administratorzy", RSAdmin.class);
        }
    }

    public static class GUsers extends RightsScheme {

        public GUsers() {
            super("gusers", "Użytkownicy", RSAdmin.class);
            super.refuse(RCron.class, RUsers.class, RConfig.class);
        }
    }

    public static class GTesters extends RightsScheme {

        public GTesters() {
            super("gtesters", "Testerzy", RTest.class);
        }
    }

}
