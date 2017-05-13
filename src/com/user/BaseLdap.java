package com.user;

import com.config.CAuthorization;
import com.config.CAuthorization.CLDAP;
import com.utils.Utils;
import com.utils.Is;
import com.exceptions.CoreException;
import com.json.Escape;
import com.servlet.Handlers;
import com.utils.StrUtils;

import static com.config.CAuthorization.CLDAP.*;
import com.lang.LUser;
import com.utils.date.time.Unit;

import java.util.*;
import javax.naming.*;
import javax.naming.directory.*;

public class BaseLdap {

    public final Properties environment = new Properties();

    public String domain;

    public static BaseLdap newInstance() {
        try {
            return Handlers.ldap.getConstructor().newInstance();
        } catch (Throwable ex) {
            throw new CoreException(ex);
        }
    }

    public static BaseLdap newInstance(String url, String domain, String username, String password) {
        try {
            return Handlers.ldap.getConstructor(String.class, String.class, String.class, String.class)
                    .newInstance(url, domain, username, password);
        } catch (Throwable ex) {
            throw new CoreException(ex);
        }
    }

    public BaseLdap() throws AuthenticationException {
        this(null, null, null, null);
    }

    public BaseLdap(String url, String domain, String username, String password)
            throws AuthenticationException {

        this.domain = domain != null ? domain
                : CAuthorization.CLDAP.domain.value();

        if (username == null || username.isEmpty() || password == null || password.isEmpty()) {
            username = adminUsername.value("");
            password = adminPassword.value("");
        }

        if (username == null || username.isEmpty() || password == null || password.isEmpty()) {
            BaseUserData user = BaseUserData.getCurrentUser();
            if (user != null) {
                username = user.username;
                password = user.passPlain;
            }
        }

        if (timeout.value() != null)
            environment.put("com.sun.jndi.ldap.connect.timeout",
                    Long.toString(timeout.value().getTime(Unit.MILLISECONDS)));
        environment.put(Context.INITIAL_CONTEXT_FACTORY, "com.sun.jndi.ldap.LdapCtxFactory");
        environment.put(Context.PROVIDER_URL, url != null && !url.isEmpty()
                ? url : CAuthorization.CLDAP.url.value());
        environment.put(Context.SECURITY_AUTHENTICATION, "simple");
        environment.put(Context.SECURITY_PRINCIPAL, "uid=" + StrUtils.encodeURIComponent(username)
                + "," + this.domain);
        environment.put(Context.SECURITY_CREDENTIALS, Utils.coalesce(password, ""));
    }

    public Map<String, String> getAttributes(String username) throws NamingException {

        Map<String, String> result = new LinkedHashMap<>();
        DirContext context = getContext();
        Attributes attribs = context.getAttributes("uid="
                + StrUtils.encodeURIComponent(username) + "," + this.domain);
        NamingEnumeration<String> ids = attribs.getIDs();
        while (ids.hasMore()) {
            String id = ids.next();
            Object value = attribs.get(id).get();
            result.put(id, value != null ? value.toString() : null);
        }
        return result;
    }

    public DirContext getContext() throws NamingException {
        return new InitialDirContext(environment);
    }

    /**
     * Formatowanie listy użytkowników zwróconej przez LDAP-a. Przykładowa
     * postać "cn=Imie.Nazwisko", docelowo: "imie.nazwisko"
     *
     * @param users
     */
    public List<String> getUsers() throws NamingException {

        List<String> list = new LinkedList<>();

        if (!CLDAP.enabled.value(false)
                || CLDAP.listUsersFilter.value("").isEmpty())
            return list;

        NamingEnumeration<SearchResult> search = BaseLdap.newInstance()
                .search(CLDAP.listUsersFilter.value(""));

        while (search.hasMore()) {

            SearchResult next = search.next();

            String name = next.getName();

            if (name != null && name.contains(".")) {
                if (name.contains("="))
                    name = name.substring(name.indexOf("=") + 1);
                list.add(name.trim().toLowerCase());
            }
        }
        Collections.sort(list);
        return list;
    }

    public Map<String, Map<String, Object>> list(boolean inclAttributes) throws NamingException {
        Map<String, Map<String, Object>> result = new HashMap<>();

        DirContext context = getContext();
        NamingEnumeration<NameClassPair> list = context.list(domain);
        while (list.hasMore()) {
            NameClassPair pair = list.next();
            if (pair == null || pair.getName() == null)
                continue;

            Map<String, Object> map = new HashMap<>();
            result.put(pair.getName(), map);

            if (!inclAttributes)
                continue;

            Attributes attributes = context.getAttributes(pair.getName() + "," + domain);
            NamingEnumeration<String> ids = attributes.getIDs();
            while (ids.hasMore()) {
                String id = ids.next();
                map.put(id, attributes.get(id));
            }

        }
        return result;
    }

    /**
     *
     * @param filter np "(objectClass=person)"
     * @return
     */
    public NamingEnumeration<SearchResult> search(String filter) throws NamingException {
        SearchControls searchControls = new SearchControls();
        searchControls.setSearchScope(SearchControls.ONELEVEL_SCOPE);
        searchControls.setTimeLimit(30000);
        return getContext().search(domain, filter, searchControls);
    }

    public static Map<String, Object> attributesToMap(Attributes attributes) throws NamingException {

        Map<String, Object> map = new LinkedHashMap<>();

        NamingEnumeration<? extends Attribute> all = attributes.getAll();
        while (all.hasMore()) {
            Attribute a = all.next();
            map.put(a.getID(), a.get());
        }
        return map;
    }

    @Override
    public String toString() {
        return environment.getProperty(Context.PROVIDER_URL);
    }

    /**
     * Wypełnij dane użytkownika
     *
     * @param user
     * @throws AuthenticationException
     * @throws NamingException
     */
    public void getUserInfo(BaseUserData user) throws AuthenticationException, NamingException {
        if (user == null || user.username == null || !CLDAP.enabled.value(false))
            return;

        NamingEnumeration<SearchResult> results
                = BaseLdap.newInstance().search("uid=" + StrUtils.encodeURIComponent(user.username));

        if (!results.hasMore())
            throw new Error(LUser.LDAP_USER_NOT_FOUND.toString(user.username));

        SearchResult next = results.next();
        Attributes attributes = next.getAttributes();
        NamingEnumeration<String> ids = attributes.getIDs();
        while (ids.hasMore()) {
            String id = ids.next();
            Object value = attributes.get(id).get();
            user.ldapAttributes.put(id, value != null ? value.toString() : null);
        }
    }

    public SearchResult checkUserName(String username) throws Exception {
        if (username == null || !CLDAP.enabled.value(false))
            return null;

        NamingEnumeration<SearchResult> results
                = BaseLdap.newInstance().search("uid=" + StrUtils.encodeURIComponent(username));

        if (!results.hasMore())
            throw new Error(LUser.LDAP_USER_NOT_FOUND.toString(username));

        return results.next();
    }

}
