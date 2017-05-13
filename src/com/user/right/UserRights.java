package com.user.right;

import com.user.RightsScheme;
import com.utils.Utils;
import com.utils.Is;
import com.context.AppContext;
import com.exceptions.http.Http403ForbiddenException;
import com.exceptions.http.Http405MethodNotAllowed;
import com.servlet.controller.Controller;
import com.servlet.interfaces.Endpoint;
import com.servlet.requests.HttpRequest;
import com.user.*;
import com.user.pages.PLogin;
import com.utils.Is;
import com.utils.collections.Strings;
import java.lang.reflect.Method;
import java.util.*;

/**
 * @author Miłosz Ziernik
 * @date 10 grudnia 2015
 * @encoding UTF-8
 */
public class UserRights implements Iterable<UserRight> {

    public final BaseUserData user;
    public final Set<UserRight> allowed = new LinkedHashSet<>();
    public final Set<UserRight> denied = new LinkedHashSet<>();

    public final LinkedHashSet<RightsScheme> groups = new LinkedHashSet<>();

    public UserRights addGroup(RightsScheme group) {
        if (group != null)
            groups.add(group);
        return this;
    }

    public UserRights(BaseUserData user) {
        this.user = user;
    }

    public UserRights allow(Class<? extends UserRight> right) {
        return allow(UserRight.get(right));
    }

    public UserRights allow(UserRight right) {
        if (right != null)
            allowed.add(right);
        return this;
    }

    public UserRights denny(Class<? extends UserRight> right) {
        return denny(UserRight.get(right));
    }

    public UserRights denny(UserRight right) {
        if (right != null)
            denied.add(right);
        return this;
    }

    /**
     * Czy posiada uprawnienie do przynajmniej jednego z wymaganych
     *
     * @param rights
     * @return
     */
    public boolean has(Class<? extends UserRight>... rights) {
        return has(Arrays.asList(UserRight.instances(rights)));
    }

    public boolean has(Collection<? extends UserRight> rights) {
        for (UserRight usrRight : rights) {
            UserRight tmpUsrRight = usrRight;
            boolean deniedDetected = false;

            while (tmpUsrRight != null) {
                if (denied.contains(tmpUsrRight)) {
                    deniedDetected = true;
                    break;
                }

                if (allowed.contains(tmpUsrRight))
                    return true;

                tmpUsrRight = tmpUsrRight.parent;
            }

            if (!deniedDetected)
                for (RightsScheme usrGroup : user.rights.groups)
                    if (usrGroup.has(usrRight.getClass()))
                        return true;
        }

        return false;
    }

    public boolean hasRootRights() {
        return user.isRoot() || UserRights.this.has(RRoot.class);
    }

    public boolean isGroupMember(RightsScheme... groups) {
        if (groups == null || groups.length == 0)
            return false;
        for (RightsScheme gr : groups)
            if (this.groups.contains(gr))
                return true;

        return false;
    }

    /**
     * Czy użytkownik ma prawa dostepu do danej strony
     */
    public boolean has(Class<? extends Controller> cls) {
        Endpoint ipage = cls.getAnnotation(Endpoint.class
        );

        if (ipage == null || !ipage.auth())
            return true;

        // root ma prawa do wszystkiego
        if (hasRootRights())
            return true;

        for (PagePermission p : PagePermission.all)
            if (p.page == cls) {
                if (p.userName != null && p.userName.equalsIgnoreCase(user.username))
                    return p.allow;

                for (RightsScheme group : groups)
                    if (p.groupName != null && p.groupName.equalsIgnoreCase(group.key))
                        return p.allow;
            }

        // jeśli login użytkownika jest na liście to zezwól
        if (ipage.rights().length > 0) {
            for (Class<? extends UserRight> right : ipage.rights())
                if (has(right))
                    return true;
        } else // jesli nie ma narzuconych grup, to zazwól
            return true;

        return false;
    }

    @Override
    public String toString() {

        Strings lst = new Strings();

        for (UserRight r : UserRight.all) {
            if (allowed.contains(r))
                lst.add("+" + r.key);

            if (denied.contains(r))
                lst.add("-" + r.key);
        }
        return lst.toString(", ");
    }

    public boolean checkMethodRights(Controller page, Method met, Endpoint ann)
            throws Http405MethodNotAllowed, Http403ForbiddenException, Exception {

        HttpRequest http = page.http();

        if (ann.devOnly() && !AppContext.devMode)
            throw new Http403ForbiddenException();

        if (!Is.in(http.getMethod(), ann.methods()))
            throw new Http405MethodNotAllowed(http);
        // root ma prawa do wszystkiego
        if (http.session.user.rights.hasRootRights())
            return true;
        // jeśli metoda nie wymaga autoryzacji
        if (!ann.auth())
            return true;

        if (!http.session.user.authorized || http.session.user.status != UserStatus.active)
            try {
                if (!BaseAuthHandler.newInstance().authorize(page, http.session.user, false))
                    return false;
            } catch (Exception e) {
                PLogin.redirect(page);
                return false;
            }

        for (PagePermission p : PagePermission.all)
            if (p.page == page.getClass()
                    && p.method != null
                    && p.method.equals(met.getName())) {

                if (!p.allow && p.userName != null
                        && p.userName.equalsIgnoreCase(http.session.user.username))
                    throw new Http403ForbiddenException();

                for (RightsScheme group : groups)
                    if (!p.allow && p.groupName != null
                            && p.groupName.equalsIgnoreCase(group.name))
                        throw new Http403ForbiddenException();
            }

        // jeśli login użytkownika jest na liście to zezwól
        if (ann.rights().length > 0) {
            if (http.session.user.rights.has(ann.rights()))
                return true;
        } else // jesli nie ma narzuconych grup, to zazwól
            return true;

        throw new Http403ForbiddenException();
    }

    @Override
    public Iterator<UserRight> iterator() {
        return getComputed().iterator();
    }

    public boolean isEmpty() {
        throw new UnsupportedOperationException("Not supported yet."); //To change body of generated methods, choose Tools | Templates.
    }

    public void check(UserRight root) {
        throw new UnsupportedOperationException("Not supported yet."); //To change body of generated methods, choose Tools | Templates.
    }

    protected LinkedList<UserRight> getComputed() {
        LinkedList<UserRight> list = new LinkedList<>();
        for (UserRight right : allowed)
            list.addAll(right.getAll());

        return list;
    }

}
