package com.user;

import com.servlet.controller.Page;
import java.util.LinkedHashSet;
import java.util.Set;

/**
 * KLasa umożliwia ręczne zarządzanie uprawnieniami stron i metod
 *
 * @author User
 */
// TODO Dodać do konfiguracji możliwość definiowania zależności Strona - Użytkownik/grupa
public class PagePermission {

    public final static Set<PagePermission> all = new LinkedHashSet<>();

    public final Class<? extends Page> page;
    public final String method;
    public final String userName;
    public final String groupName;
    public final boolean allow;

    private PagePermission(final Class<? extends Page> page, final String method,
            boolean allow, final String userName, String groupName) {
        this.page = page;
        this.method = method;
        this.userName = userName;
        this.groupName = groupName;
        this.allow = allow;
    }

    public static PagePermission allowUser(final Class<? extends Page> page, final String method,
            final String userName) {
        PagePermission exclude = new PagePermission(page, method, true, userName, null);
        all.add(exclude);
        return exclude;
    }

    public static PagePermission rejectUser(final Class<? extends Page> page, final String method,
            final String userName) {
        PagePermission exclude = new PagePermission(page, method, false, userName, null);
        all.add(exclude);
        return exclude;
    }

    public static PagePermission allowGroup(final Class<? extends Page> page, final String method,
            final String groupName) {
        PagePermission exclude = new PagePermission(page, method, true, null, groupName);
        all.add(exclude);
        return exclude;
    }

    public static PagePermission rejectGroup(final Class<? extends Page> page, final String method,
            final String groupName) {
        PagePermission exclude = new PagePermission(page, method, false, null, groupName);
        all.add(exclude);
        return exclude;
    }
}
