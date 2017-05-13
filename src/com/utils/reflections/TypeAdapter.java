package com.utils.reflections;

import com.exceptions.ThrowableException;
import com.json.JArray;
import com.json.JElement;
import com.json.JObject;
import com.json.JSON;
import com.lang.LUtil;
import com.utils.Utils;
import com.utils.Is;
import com.utils.collections.Strings;
import com.utils.date.TDate;
import static com.utils.reflections.TClass.sep;
import java.lang.reflect.Array;
import java.lang.reflect.Constructor;
import java.lang.reflect.Field;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.*;

/**
 * Klasa zbiorcza zajmująca się parsowaniem róznego rodzaju obiektów
 */
public class TypeAdapter<T> {

    public String mapSeparator = ":";
    public final Class<T> cls;
    public final TClass<T> tcls;

    public TypeAdapter(Class<T> destinationClass) {
        this.cls = destinationClass;
        this.tcls = new TClass<>(destinationClass);
    }

    public static interface IDeserializer {

        public Object parse(LinkedList<String> arguments) throws Exception;
    }

    public final static Map<Class<?>, IDeserializer> deserializers = new LinkedHashMap<>();

    public boolean isSupporterd() {
        return isSupporterd(cls);
    }

    public static boolean isSupporterd(Class<?> cls) {
        if (cls.isPrimitive())
            return true;

        if (Is.in(cls, Boolean.class, Byte.class, Short.class, Integer.class,
                Long.class, Float.class, Double.class, Character.class,
                String.class, Enum.class, Date.class, TDate.class))
            return true;

        TClass<?> clazz = new TClass<>(cls);
        if (clazz.instanceOf(JElement.class, Collection.class, Map.class))
            return true;

        for (Constructor<?> c : cls.getDeclaredConstructors())
            if (c.getParameterCount() == 1) {
                Class<?> cp = c.getParameterTypes()[0];

                TClass<?> cc = new TClass<>(cp);

                if (cp == String.class || cp == String[].class)
                    return true;

                if (cc.instanceOf(Collection.class)
                        && cc.generic.length == 1)
                    return true;
            }

        return true;
    }

    public T collection(Object... values) {
        return collection(Arrays.asList(values), null);
    }

    public T process(Object value) {
        if (value == null)
            return null;

        if (value instanceof Collection)
            return collection((Collection) value);

        return single(value, null);
    }

    public T collection(Collection<? extends Object> values, Object instance) {

        if (cls.isArray()) {
            Class<?> cType = cls.getComponentType();
            Object array = Array.newInstance(cType, values.size());

            int idx = 0;
            for (Object s : values)
                Array.set(array, idx++, collection(new TClass<T>(cType), s));
            return (T) array;
        }

        if (cls == Strings.class)
            return (T) new Strings().addAll(values);

        {
            Collection collection = null;

            if (cls == List.class || cls == Collection.class)
                collection = new LinkedList();

            if (collection == null && cls == Set.class)
                collection = new LinkedHashSet();

            if (collection == null && Collection.class.isAssignableFrom(cls))
                collection = (Collection) tcls.newInstance(instance);

            if (collection != null) {

                for (Object s : values)
                    collection.add(tcls.generic.length == 1
                            ? collection(new TClass<>(tcls.generic[0]), s) : s);

                return (T) collection;
            }
        }
        {

            if (Map.class.isAssignableFrom(cls)) {
                Map map = tcls.isAbstract()
                        ? new LinkedHashMap()
                        : (Map) tcls.newInstance(instance);

                for (Object key : values) {
                    String skey = Utils.toString(key);

                    String value = null;
                    if (skey.contains(mapSeparator)) {
                        value = skey.substring(skey.indexOf(":") + 1);
                        skey = skey.substring(0, skey.indexOf(":"));
                    }

                    if (tcls.generic.length == 2)
                        map.put(new TypeAdapter<>(tcls.generic[0]).single(skey, instance),
                                new TypeAdapter<>(tcls.generic[1]).single(value, instance));
                    else
                        map.put(skey, value);
                    return (T) map;
                }

            }
        }
        Iterator<? extends Object> itr = values.iterator();
        Object value = itr.hasNext() ? values.iterator().next() : null;
        return single(value, instance);
    }

    public T single(Object value, Object instance) {

        if (value == null)
            return null;

        if (cls.isAssignableFrom(value.getClass()))
            return (T) value;

        if (cls == Object.class)
            return (T) value;

        String strVal = Utils.toString(value);

        if (cls == String.class)
            return (T) value;

        if (cls == Boolean.TYPE || cls == Boolean.class)
            return (T) Boolean.valueOf(strVal);

        if (cls == Byte.TYPE || cls == Byte.class)
            return (T) Byte.valueOf(strVal);

        if (cls == Short.TYPE || cls == Short.class)
            return (T) Short.valueOf(strVal);

        if (cls == Integer.TYPE || cls == Integer.class)
            return (T) Integer.valueOf(strVal);

        if (cls == Long.TYPE || cls == Long.class)
            return (T) Long.valueOf(strVal);

        if (cls == Float.TYPE || cls == Float.class)
            return (T) Float.valueOf(sep(strVal));

        if (cls == Double.TYPE || cls == Double.class)
            return (T) Double.valueOf(sep(strVal));

        if (cls == Character.TYPE || cls == Character.class) {
            if (strVal.length() != 1)
                throw new RuntimeException(LUtil.INVALID_CHAR_VALUE.toString());
            return (T) Character.valueOf(strVal.charAt(0));
        }

        if (Enum.class.isAssignableFrom(cls)) {
            for (Field field : ((Class) cls).getFields())
                if (field.getName().equals(value))
                    try {
                        field.setAccessible(true);
                        return (T) field.get(null);
                    } catch (Exception ex) {
                        throw new UnsupportedOperationException(ex);
                    }
            throw new RuntimeException(LUtil.INVALID_VALUE.toString(value));
        }

        if (cls == Date.class)
            try {
                if (value instanceof Number)
                    return (T) new Date(((Number) value).longValue());
                return (T) new SimpleDateFormat(TDate.FULL_MS).parse(strVal);
            } catch (ParseException ex) {
                throw new ThrowableException(ex);
            }

        if (cls == TDate.class)
            try {
                if (value instanceof Number)
                    return (T) new TDate(((Number) value).longValue());
                return (T) new TDate(strVal);
            } catch (ParseException ex) {
                throw new ThrowableException(ex);
            }

        if (JObject.class.isAssignableFrom(cls))
            return (T) JObject.parse(strVal);

        if (JArray.class.isAssignableFrom(cls))
            return (T) JArray.parse(strVal);

        if (JElement.class.isAssignableFrom(cls))
            return (T) JSON.parse(strVal);

        if (!tcls.isAbstract())
            try {
                Constructor<?>[] constructors = cls.getDeclaredConstructors();
                for (Constructor<?> c : constructors)
                    if (c.getParameterCount() == 1 && isSupporterd(c.getParameterTypes()[0]))
                        return (T) collection((TClass<T>) new TClass<>(
                                c.getParameterTypes()[0]), value, instance);

            } catch (Exception ex) {
                throw new ThrowableException(ex);
            }

        throw new UnsupportedOperationException(LUtil.CANT_DESERIALIZE_CLASS.toString(tcls.getFullName()));
    }
}