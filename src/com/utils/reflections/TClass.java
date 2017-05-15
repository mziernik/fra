package com.utils.reflections;

import com.exceptions.CoreException;
import com.utils.collections.TList;
import java.lang.annotation.Annotation;
import java.lang.reflect.*;
import java.util.Arrays;
import java.util.Collection;
import java.util.LinkedList;
import java.util.List;

/**
 * @author Miłosz Ziernik
 * @date 15 grudnia 2015
 * @encoding UTF-8
 */
public class TClass<T> implements TReflection {

    public final Class<T> raw;
    public final Type type;
    public final Class[] generic;

    public TClass(Type type) {
        this(type instanceof Class ? (Class<T>) type : null, type);
        if (raw == null)
            throw new UnsupportedOperationException(type != null ? type.toString() : null);
    }

    public TClass(String className) throws ClassNotFoundException {
        this(Class.forName(className, false, ClassLoader.getSystemClassLoader()));
    }

    public TClass(Class<T> clazz) {
        this(clazz, clazz);
    }

    @Override
    public String toString() {
        return "TClass(class: " + raw + ", type: " + type + ")";
    }

    public TClass(Field f) {
        this((Class<T>) f.getType(), f.getGenericType());
    }

    public TClass(Class<T> clazz, Type type) {
        this.raw = clazz;
        this.type = type;

        List<Class<?>> list = new LinkedList<>();
        if (type instanceof ParameterizedType)
            for (Type t : ((ParameterizedType) type).getActualTypeArguments())
                if (t instanceof Class)
                    list.add((Class<?>) t);

        generic = list.toArray(new Class[0]);
    }

    /**
     * Zwraca wszystkie pola danej klasy
     *
     * @param includeSuperclass Czy mają być również uwzględniane pola klas
     * macierzystych
     * @param superClasses Klasy, które muszą rozszerzać pola
     * @return
     */
    public TList<TField> getDeclaredFields(boolean includeSuperclass, Class<?>... superClasses) {
        TList<TField> list = new TList<>();
        Class<?> cls = raw;
        while (cls != null) {
            if (cls.isAssignableFrom(cls))
                for (Field f : cls.getDeclaredFields()) {

                    boolean ok = superClasses.length == 0;
                    Class<?> type = f.getType();
                    for (Class<?> c : superClasses)
                        ok |= c.isAssignableFrom(type);
                    if (ok)
                        list.add(new TField(f));
                }

            if (!includeSuperclass)
                return null;
            cls = cls.getSuperclass();
        }
        return list;
    }

    public Field getDeclaredField(String name, boolean includeSuperclass) {
        Class<?> cls = raw;
        while (cls != null) {
            for (Field f : cls.getDeclaredFields())
                if (f.getName().equals(name))
                    return f;

            if (!includeSuperclass)
                return null;
            cls = cls.getSuperclass();
        }
        return null;
    }

    public List<Method> getDeclaredMethods(boolean includeSuperclass) {
        List<Method> list = new LinkedList<>();
        Class<?> cls = raw;
        while (cls != null) {
            list.addAll(Arrays.asList(cls.getDeclaredMethods()));
            if (!includeSuperclass)
                return null;
            cls = cls.getSuperclass();
        }
        return list;
    }

    public boolean isAnonymous() {
        return raw.isAnonymousClass();
    }

    public boolean isPrimitive() {
        return raw == Boolean.TYPE
                || raw == Byte.TYPE
                || raw == Short.TYPE
                || raw == Integer.TYPE
                || raw == Long.TYPE
                || raw == Float.TYPE
                || raw == Double.TYPE
                || raw == Character.TYPE;
    }

    public boolean isSimple() {
        return raw == Boolean.class
                || raw == Byte.class
                || raw == Short.class
                || raw == Integer.class
                || raw == Long.class
                || raw == Float.class
                || raw == Double.class
                || raw == Character.class
                || raw == String.class;
    }

    public boolean isSimpleArray() {
        return raw == boolean[].class
                || raw == Boolean[].class
                || raw == byte[].class || raw == Byte[].class
                || raw == short[].class || raw == Short[].class
                || raw == int[].class || raw == Integer[].class
                || raw == long[].class || raw == Long[].class
                || raw == float[].class || raw == Float[].class
                || raw == double[].class || raw == Double[].class
                || raw == char[].class || raw == Character[].class
                || raw == String[].class;
    }

    public T newInstance(Object parent, Object... args) {
        return (T) new TExecutable(parent, raw.getDeclaredConstructors())
                .invoke(args);
    }

    @Override
    public Object invoke(Object parent, Object... args) {
        return newInstance(parent, args);
    }

    public T deserialize(String... values) {
        return (T) deserialize(Arrays.asList(values), null);
    }

    public T deserialize(Collection<? extends Object> values, Object instance) {
        return new TypeAdapter<T>(raw).collection(values, instance);
    }

    public Class<?>[] getClassGenericTypes() {
        return TReflection.getClassGenericTypes(raw.getGenericInterfaces());
    }

    /*
     public static void setFieldArrayValue(Field field, Object obj, String[] values)
     throws IllegalArgumentException, IllegalAccessException, ParseException {

     if (field == null || obj == null)
     return;

     Class<?> type = field.getType();

     Object current = field.get(obj);

     if (values == null) {
     field.set(obj, null);
     return;
     }

     if (type == boolean[].class) {
     boolean[] arr = new boolean[values.length];
     for (int i = 0; i < values.length; i++) {
     arr[i] = Boolean.parseBoolean(values[i]);
     field.set(obj, arr);
     }
     return;
     }
     if (type == Boolean[].class) {
     Boolean[] arr = new Boolean[values.length];
     for (int i = 0; i < values.length; i++) {
     arr[i] = Boolean.parseBoolean(values[i]);
     field.set(obj, arr);
     }
     return;
     }
     if (type == short[].class) {
     short[] arr = new short[values.length];
     for (int i = 0; i < values.length; i++) {
     arr[i] = Short.parseShort(values[i]);
     field.set(obj, arr);
     }
     return;
     }
     if (type == Short[].class) {
     Short[] arr = new Short[values.length];
     for (int i = 0; i < values.length; i++) {
     arr[i] = Short.parseShort(values[i]);
     field.set(obj, arr);
     }
     return;
     }
     if (type == int[].class) {
     int[] arr = new int[values.length];
     for (int i = 0; i < values.length; i++) {
     arr[i] = Integer.parseInt(values[i]);
     field.set(obj, arr);
     }
     return;
     }
     if (type == Integer[].class) {
     Integer[] arr = new Integer[values.length];
     for (int i = 0; i < values.length; i++) {
     arr[i] = Integer.parseInt(values[i]);
     field.set(obj, arr);
     }
     return;
     }
     if (type == long[].class) {
     long[] arr = new long[values.length];
     for (int i = 0; i < values.length; i++) {
     arr[i] = Long.parseLong(values[i]);
     field.set(obj, arr);
     }
     return;
     }
     if (type == Long[].class) {
     Long[] arr = new Long[values.length];
     for (int i = 0; i < values.length; i++) {
     arr[i] = Long.parseLong(values[i]);
     field.set(obj, arr);
     }
     return;
     }
     if (type == float[].class) {
     float[] arr = new float[values.length];
     for (int i = 0; i < values.length; i++) {
     arr[i] = Float.parseFloat(values[i]);
     field.set(obj, arr);
     }
     return;
     }
     if (type == Float[].class) {
     Float[] arr = new Float[values.length];
     for (int i = 0; i < values.length; i++) {
     arr[i] = Float.parseFloat(values[i]);
     field.set(obj, arr);
     }
     return;
     }
     if (type == double[].class) {
     double[] arr = new double[values.length];
     for (int i = 0; i < values.length; i++) {
     arr[i] = Double.parseDouble(values[i]);
     field.set(obj, arr);
     }
     return;
     }
     if (type == Double[].class) {
     Double[] arr = new Double[values.length];
     for (int i = 0; i < values.length; i++) {
     arr[i] = Double.parseDouble(values[i]);
     field.set(obj, arr);
     }
     return;
     }
     if (type == char[].class) {
     char[] arr = new char[values.length];
     for (int i = 0; i < values.length; i++) {
     if (values[i].length() != 1)
     throw new ParseException("Nieprawidłowa wartość char", 0);
     arr[i] = values[i].charAt(0);
     field.set(obj, arr);
     }
     return;
     }
     if (type == Character[].class) {
     Character[] arr = new Character[values.length];
     for (int i = 0; i < values.length; i++) {
     if (values[i].length() != 1)
     throw new ParseException("Nieprawidłowa wartość char", 0);
     arr[i] = values[i].charAt(0);
     field.set(obj, arr);
     }
     return;
     }
     if (type == String[].class) {
     String[] arr = new String[values.length];
     for (int i = 0; i < values.length; i++) {
     arr[i] = values[i];
     field.set(obj, arr);
     }
     }

     }
     */
    /**
     * Konwersja separatora dziesietnego
     */
    static String sep(String value) {
        if (value == null)
            return null;
        return value.replace(",", ".");
    }

    public boolean instanceOf(Class<?>... childClasses) {
        if (childClasses == null)
            return false;

        for (Class<?> c : childClasses)
            if (c.isAssignableFrom(raw))
                return true;

        /*     Class<?> current = raw;
        if (current.isPrimitive())
            current = toSimple();

        while (current != null) {
            for (Class<?> c : childClasses) {
                if (c.isPrimitive())
                    c = new TClass(c).toSimple();
                if (c == current)
                    return true;
            }

            Class[] intfs = current.getInterfaces();
            if (intfs != null)
                for (Class<?> c : childClasses)
                    for (Class<?> i : intfs) {
                        if (c.isPrimitive())
                            c = new TClass(c).toSimple();
                        if (c == i)
                            return true;
                    }

            current = current.getSuperclass();
        }*/
        return false;
    }

    public Class<T> toSimple() {
        if (raw == null)
            return null;
        if (raw == Boolean.TYPE)
            return (Class<T>) Boolean.class;
        if (raw == Byte.TYPE)
            return (Class<T>) Byte.class;
        if (raw == Short.TYPE)
            return (Class<T>) Short.class;
        if (raw == Integer.TYPE)
            return (Class<T>) Integer.class;
        if (raw == Long.TYPE)
            return (Class<T>) Long.class;
        if (raw == Float.TYPE)
            return (Class<T>) Float.class;
        if (raw == Double.TYPE)
            return (Class<T>) Double.class;
        if (raw == Character.TYPE)
            return (Class<T>) Character.class;
        return raw;

    }

    /**
     * Czy klasa ma zadeklarowany typ generyczny, np
     *
     * class Klasa<Integer> {}
     *
     * hasGenericType(Klasa.class, Number.class) == true
     *
     * @param cls
     * @param genericType
     * @return
     */
    public boolean hasGenericType(Class genericType) {
        for (Type t : generic)
            if (t instanceof Class && new TClass(t).instanceOf(genericType))
                return true;
        return false;
    }

    @Override
    public int getModifiers() {
        return raw.getModifiers();
    }

    @Override
    public void checkModifiers(int... modifiers) throws CoreException {
        Reflections.checkModifiers("Klasa " + raw.getName(), raw.getModifiers(), modifiers);
    }

    @Override
    public String getFullName() {
        return raw.getName();
    }

    @Override
    public Class<?> getDeclaringClass() {
        return raw.getDeclaringClass();
    }

    @Override
    public <A extends Annotation> A getAnnotation(Class<A> annotationClass) {
        return raw.getAnnotation(annotationClass);
    }

    @Override
    public String getName() {
        return raw.getName();
    }

    @Override
    public Class<?> getReturnType() {
        return raw;
    }

}
