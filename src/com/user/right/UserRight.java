package com.user.right;

import com.dev.Dev;
import com.context.index.Index;
import com.lang.core.LString;
import java.util.*;

/**
 * @author Miłosz Ziernik
 * @date 10 grudnia 2015
 * @encoding UTF-8
 */
public abstract class UserRight implements Iterable<UserRight> {

    public final String key;
    public final LString name;
    public String description;
    public UserRight parent;
    private final List<UserRight> children = new LinkedList<>();
    final static LinkedHashSet<UserRight> all = new LinkedHashSet<>();

    public final static RRoot root = new RRoot();

    @Override
    public String toString() {
        return name.toString();
    }

    protected UserRight(String key, LString name) {
        this.key = key;
        this.name = name;
    }

    /**
     * Zwraca wszystkie uprawnienia wraz z dziedziczonymi
     *
     * @return
     */
    public LinkedHashSet<UserRight> getAll() {
        LinkedHashSet<UserRight> list = new LinkedHashSet<>();
        list.add(this);
        list.addAll(children);
        return list;
    }

    public static <T extends UserRight> T get(Class<T> right) {
        if (right == null)
            return null;
        synchronized (all) {
            for (UserRight r : all)
                if (r.getClass() == right)
                    return (T) r;
        }
        Dev.warning("Klasa uprawnień " + right.getName() + " nie jest zarejestrowana");
        return null;
    }

    public static UserRight get(String key) {
        synchronized (all) {
            for (UserRight r : all)
                if (r.key.equals(key))
                    return r;
        }
        return null;
    }

    public static void onIndexLoaded() throws Exception {

        for (Class<? extends UserRight> cls : Index.rights)
            all.add(cls.newInstance());

        for (UserRight right : all)
            for (UserRight r : all)
                if (r.getClass().getSuperclass() == right.getClass()) {
                    r.parent = right;
                    right.children.add(r);
                }

    }

    @Override
    public Iterator<UserRight> iterator() {
        return children.iterator();
    }

    public static UserRight[] instances(Class<? extends UserRight>... rights) {
        LinkedList<UserRight> list = new LinkedList<>();
        for (Class<? extends UserRight> cc : rights) {
            UserRight right = UserRight.get(cc);
            if (right != null)
                list.add(right);
        }
        return list.toArray(new UserRight[0]);
    }

    @Override
    public int hashCode() {
        int hash = 7;
        hash = 19 * hash + Objects.hashCode(this.key);
        return hash;
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj)
            return true;
        if (obj == null)
            return false;
        if (getClass() != obj.getClass())
            return false;
        final UserRight other = (UserRight) obj;
        if (!Objects.equals(this.key, other.key))
            return false;
        return true;
    }
}
