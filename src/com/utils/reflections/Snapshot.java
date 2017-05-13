package com.utils.reflections;

import com.exceptions.ServiceException;
import com.exceptions.ThrowableException;
import com.lang.LUtil;
import com.utils.collections.Triple;
import java.lang.reflect.Field;
import java.lang.reflect.Method;
import java.lang.reflect.Modifier;
import java.util.Map.Entry;
import java.util.*;

public class Snapshot<T> {

    public final T object;
    private final Map<Field, Object> map = new HashMap<>();

    public Snapshot(T object, String... fields) {
        this.object = object;

        Class<?> cls = object.getClass();

        while (cls != null && cls != Object.class) {
            final LinkedList<Field> ffields = new LinkedList<>();

            if (fields != null && fields.length > 0)
                for (String s : fields) {
                    Field field = new TClass<>(cls).getDeclaredField(s, true);
                    if (field == null)
                        throw new ServiceException(LUtil.CANT_FIND_FIELD.toString(s));
                    ffields.add(field);
                }

            if (ffields.isEmpty())
                ffields.addAll(Arrays.asList(cls.getDeclaredFields()));

            for (Field f : ffields)
                try {
                    int mods = f.getModifiers();
                    if (Modifier.isTransient(mods) || Modifier.isStatic(mods))
                        continue;
                    f.setAccessible(true);

                    Object value = f.get(object);

                    if (value instanceof Cloneable && !value.getClass().isArray()) {
                        Method method = value.getClass().getMethod("clone");
                        method.setAccessible(true);
                        value = method.invoke(value);
                    }

                    map.put(f, value);
                } catch (Exception e) {
                    throw new ServiceException(e);
                }
            cls = cls.getSuperclass();
        }
    }

    public void revert() {
        export(object);
    }

    public void export(T dst) {
        for (Entry<Field, Object> en : map.entrySet())
            try {
                Field f = en.getKey();
                f.setAccessible(true);
                Object value = en.getValue();

                f.set(dst, value);

            } catch (SecurityException | IllegalArgumentException | IllegalAccessException e) {
                throw new ThrowableException(e);
            }

    }

    public LinkedList<Triple<Field, Object, Object>> getChanges() {
        return getChanges(null);
    }

    public LinkedList<Triple<Field, Object, Object>> getChanges(Comparator<Object> comparator) {
        LinkedList<Triple<Field, Object, Object>> list = new LinkedList<>();

        for (Map.Entry<Field, Object> en : map.entrySet())
            try {
                Field field = en.getKey();
                Object oldVal = field.get(object);
                Object newVal = en.getValue();

                if (oldVal == null && en.getValue() == null)
                    continue;

                if (comparator != null && comparator.compare(oldVal, newVal) != 0)
                    list.add(new Triple<>(field, oldVal, newVal));

                if (comparator == null && !Objects.equals(oldVal, newVal))
                    list.add(new Triple<>(field, oldVal, newVal));

            } catch (Exception e) {
                throw new ServiceException(e);
            }

        return list;
    }

}
