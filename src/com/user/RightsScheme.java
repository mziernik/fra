package com.user;

import com.utils.Utils;
import com.utils.Is;
import com.dev.Dev;
import com.context.index.Index;
import com.context.intf.ContextInitStage;
import com.utils.text.StrWriter;
import java.util.*;
import com.mlogger.Log;
import com.user.right.UserRight;
import com.context.intf.ContextInitialized;
import com.lang.LUser;
import com.utils.reflections.Revertable;

/**
 * Miłosz Ziernik 2013/06/13
 */
public class RightsScheme implements Revertable<RightsScheme>, Comparable<RightsScheme> {

    private final static LinkedHashSet<RightsScheme> all = new LinkedHashSet<>();

    //-------------------------------------------------------------
    public final String key;
    public String name;
    public String details;
    public boolean enabled = true;
    public final boolean embedded;
    public UserGroupType type;
    public boolean profile; // profil uprawnień / rola w systemie

    public final HashSet<UserRight> allowed = new HashSet<>(); // role danej grupy
    public final HashSet<UserRight> refused = new HashSet<>(); // odmówiione role

    /**
     * Usuwa wszystkie nie wbudowane grupy
     */
    public static void clearAll() {
        synchronized (all) {
            for (RightsScheme group : Utils.asList(all))
                if (!group.embedded)
                    all.remove(group);
        }
    }

    public final static LinkedList<RightsScheme> getAll() {
        synchronized (all) {
            return Utils.asList(all);
        }
    }

    public static <T extends RightsScheme> T get(Class<T> group) {
        if (group == null)
            return null;

        synchronized (all) {
            for (RightsScheme g : all)
                if (g.getClass() == group)
                    return (T) g;
        }
        Dev.warning("Klasa grupy użytkowników " + group.getName() + " nie jest zarejestrowana");
        return null;
    }

    public static void add(RightsScheme group) {
        synchronized (all) {
            all.add(group);
        }
    }

    public static void addAll(Collection<RightsScheme> groups) {
        synchronized (all) {
            all.addAll(groups);
        }
    }

    public static boolean remove(RightsScheme group) {
        synchronized (all) {
            return all.remove(group);
        }
    }

    public RightsScheme(final String key, final String name, Class<? extends UserRight>... rights) {
        if (key == null)
            throw new RuntimeException(LUser.ID_CANT_BE_EMPTY.toString());

        this.key = key.toLowerCase().trim();
        this.name = name;
        this.embedded = Index.groups.contains(getClass());

        RightsScheme ug = get(key, false);
        if (ug != null)
            throw new RuntimeException(LUser.GROUP_ALREADY_EXISTS.toString(key));

        if (rights != null)
            for (UserRight right : UserRight.instances(rights))
                this.allowed.add(right);

        all.add(this);
    }

    public RightsScheme allow(UserRight... rights) {
        if (rights != null)
            for (UserRight r : rights)
                if (r != null)
                    this.allowed.add(r);
        return this;
    }

    public RightsScheme allow(Class<? extends UserRight>... rights) {
        if (rights != null)
            for (UserRight r : UserRight.instances(rights))
                this.allowed.add(r);
        return this;
    }

    public RightsScheme refuse(UserRight... rights) {
        if (rights != null)
            for (UserRight r : rights)
                if (r != null)
                    this.refused.add(r);
        return this;
    }

    public RightsScheme refuse(Class<? extends UserRight>... rights) {
        if (rights != null)
            for (UserRight r : UserRight.instances(rights))
                this.refused.add(r);
        return this;
    }

    public static Set<RightsScheme> asList(Iterable<String> groupsKeys, String username) {
        Set<RightsScheme> list = new TreeSet<>();
        for (String s : groupsKeys) {
            RightsScheme gr = get(s, true, username);
            if (gr != null)
                list.add(gr);
        }
        return list;
    }

    public static Set<RightsScheme> asList(String... groupsIds) {
        return asList(Arrays.asList(groupsIds), null);
    }

    public boolean has(Class<? extends UserRight>... rights) {
        return has(Arrays.asList(UserRight.instances(rights)));
    }

    public boolean has(Collection<? extends UserRight> rights) {
        for (UserRight usrRight : rights) {
            UserRight tmpUsrRight = usrRight;

            while (tmpUsrRight != null) {
                if (refused.contains(tmpUsrRight))
                    break;

                if (allowed.contains(tmpUsrRight))
                    return true;

                tmpUsrRight = tmpUsrRight.parent;
            }
        }
        return false;
    }

    @Override
    public String toString() {
        return key;
    }

    public static RightsScheme get(final String key, boolean checkExisting) {
        return get(key, checkExisting, null);
    }

    /**
     * Pobranie i weryfikacja istnienia grupy
     *
     * @param id
     * @param key
     * @param checkExisting
     * @return
     */
    public static RightsScheme get(final String key, boolean checkExisting, String username) {

        for (RightsScheme group : all)
            if (group.key != null && group.key.equalsIgnoreCase(key))
                return group;

        if (checkExisting)
            Log.warning("Service", "Niezdefiniowana grupa "
                    + (key != null ? "\"" + key + "\"" : "")
                    + (username != null ? ", użytkownik " + username : ""));
        return null;
    }

    @Override
    public int compareTo(RightsScheme o) {
        return key.compareTo(o.key);
    }

    /**
     * Zwraca wszystkie role grupy uwzględniając odrzucone
     *
     * @return
     */
    public Set<UserRight> getRoles() {
        final TreeSet<UserRight> list = new TreeSet<>();
        //   list.addAll(Role.getAll(allowed));
        //   list.removeAll(getRefusedRoles());
        return list;
    }

    /**
     * Zwraca tekstowa reprezentcję drzewa grup.
     *
     * @param intent
     * @param includeChildren
     * @param includeRoles: null: zwróci role danej grupy, true: role grupy wraz
     * z dziedziczącymi, false: nic
     * @return
     */
    public String toString(final Boolean includeRoles) {
        final StrWriter sb = new StrWriter();

        for (RightsScheme group : all) {
            sb.append(group.key);

            if (includeRoles == null || includeRoles)
                sb.append(" ").append(getRoles().toString());

            sb.append("\n");
        }

        return sb.toString().trim();

    }

    @ContextInitialized(stage = ContextInitStage.beforeUsersLoaded)
    private static void onInitialize() throws Exception {
        for (Class<? extends RightsScheme> cls : Index.groups)
            all.add(cls.newInstance());
    }
}
