package com.utils.reflections;

import com.exceptions.CoreException;
import com.exceptions.ThrowableException;
import com.lang.LUtil;
import com.mlogger.Log;
import com.utils.Path;
import com.utils.Utils;
import com.utils.Is;
import com.utils.collections.Strings;
import com.utils.hashes.Hashes;
import java.io.File;
import java.lang.annotation.Annotation;
import java.lang.reflect.*;
import java.net.URISyntaxException;
import java.util.*;

public class Reflections {

    public static boolean isSimpleField(Field field, Boolean inclArrays) {
        if (field == null)
            return false;
        return isSimpleField(field.getType(), inclArrays);
    }

    public static boolean classExtends(Class currentClass, String... childClasses) {
        for (Class<?> cls : getAllSuperClasses(currentClass, true))
            if (Is.in(cls.getName(), childClasses))
                return true;
        return false;
    }

    public static LinkedHashSet<Class<?>> getAllSuperClasses(Class<?> current, boolean includeInterfaces) {

        LinkedHashSet<Class<?>> list = new LinkedHashSet<>();
        while (current != null) {
            if (includeInterfaces) {
                Class<?>[] intfs = current.getInterfaces();
                if (intfs != null)
                    list.addAll(Arrays.asList(intfs));
            }
            current = current.getSuperclass();
            if (current != null)
                list.add(current);
        }
        return list;
    }

    @Deprecated
    public static boolean isSimpleField(Class<?> type, Boolean inclArrays) {
        if (type == null)
            return false;

        return type == Boolean.TYPE || type == Boolean.class
                || type == Byte.TYPE || type == Byte.class
                || type == Short.TYPE || type == Short.class
                || type == Integer.TYPE || type == Integer.class
                || type == Long.TYPE || type == Long.class
                || type == Float.TYPE || type == Float.class
                || type == Double.TYPE || type == Double.class
                || type == Character.TYPE || type == Character.class
                || type == String.class
                || (inclArrays && (type == boolean[].class
                || type == Boolean[].class
                || type == byte[].class || type == Byte[].class
                || type == short[].class || type == Short[].class
                || type == int[].class || type == Integer[].class
                || type == long[].class || type == Long[].class
                || type == float[].class || type == Float[].class
                || type == double[].class || type == Double[].class
                || type == char[].class || type == Character[].class
                || type == String[].class));
    }

    /*
     public static String[] getFieldValues(Field field, Object obj)
     throws IllegalArgumentException, IllegalAccessException, ParseException {
     Class<?> type = field.getType();
     Object val = field.get(obj);
     List<String> lst = new LinkedList<>();

     if (val == null) {
     } else
     if (type == boolean[].class) {
     boolean[] arr = (boolean[]) val;
     for (boolean v : arr)
     lst.add(Boolean.toString(v));
     } else
     if (type == Boolean[].class) {
     Boolean[] arr = (Boolean[]) val;
     for (Boolean v : arr)
     lst.add(Boolean.toString(v));
     } else
     if (type == byte[].class) {
     byte[] arr = (byte[]) val;
     for (byte v : arr)
     lst.add(Byte.toString(v));
     } else
     if (type == Byte[].class) {
     Byte[] arr = (Byte[]) val;
     for (Byte v : arr)
     lst.add(Byte.toString(v));
     } else
     if (type == short[].class) {
     short[] arr = (short[]) val;
     for (short v : arr)
     lst.add(Short.toString(v));
     } else
     if (type == Short[].class) {
     Short[] arr = (Short[]) val;
     for (Short v : arr)
     lst.add(Short.toString(v));
     } else
     if (type == int[].class) {
     int[] arr = (int[]) val;
     for (int v : arr)
     lst.add(Integer.toString(v));
     } else
     if (type == Integer[].class) {
     Integer[] arr = (Integer[]) val;
     for (Integer v : arr)
     lst.add(Integer.toString(v));
     } else
     if (type == float[].class) {
     float[] arr = (float[]) val;
     for (float v : arr)
     lst.add(Float.toString(v));
     } else
     if (type == Float[].class) {
     Float[] arr = (Float[]) val;
     for (Float v : arr)
     lst.add(Float.toString(v));
     } else
     if (type == double[].class) {
     double[] arr = (double[]) val;
     for (double v : arr)
     lst.add(Double.toString(v));
     } else
     if (type == Double[].class) {
     Double[] arr = (Double[]) val;
     for (Double v : arr)
     lst.add(Double.toString(v));
     } else
     if (type == char[].class) {
     char[] arr = (char[]) val;
     for (char v : arr)
     lst.add(Character.toString(v));
     } else
     if (type == Character[].class) {
     Character[] arr = (Character[]) val;
     for (Character v : arr)
     lst.add(Character.toString(v));
     } else
     if (type == String[].class) {
     String[] arr = (String[]) val;
     for (String v : arr)
     lst.add(v);
     } else
     throw new ParseException("Nieobsługiwany typ danych: " + type.getName(), 0);

     String[] res = new String[lst.size()];
     lst.toArray(res);
     return res;
     }
     */
 /*
     public static String getFieldValue(Field field, Object obj)
     throws IllegalArgumentException, IllegalAccessException {
     Class<?> type = field.getType();
     if (type == Boolean.TYPE)
     return Boolean.toString(field.getBoolean(obj));

     if (type == Byte.TYPE)
     return Byte.toString(field.getByte(obj));

     if (type == Short.TYPE)
     return Short.toString(field.getShort(obj));

     if (type == Integer.TYPE)
     return Integer.toString(field.getInt(obj));

     if (type == Long.TYPE)
     return Long.toString(field.getLong(obj));

     if (type == Float.TYPE)
     return Float.toString(field.getFloat(obj));

     if (type == Double.TYPE)
     return Double.toString(field.getDouble(obj));

     if (type == Character.TYPE)
     return Character.toString(field.getChar(obj));

     Object val = field.get(obj);

     if (type == boolean[].class)
     return Arrays.toString((boolean[]) val);
     else
     if (type == Boolean[].class)
     return Arrays.toString((Boolean[]) val);

     if (type == byte[].class)
     return Arrays.toString((byte[]) val);

     if (type == Byte[].class)
     return Arrays.toString((Byte[]) val);

     if (type == short[].class)
     return Arrays.toString((short[]) val);

     if (type == Short[].class)
     return Arrays.toString((Short[]) val);

     if (type == int[].class)
     return Arrays.toString((int[]) val);

     if (type == Integer[].class)
     return Arrays.toString((Integer[]) val);

     if (type == long[].class)
     return Arrays.toString((long[]) val);

     if (type == Long[].class)
     return Arrays.toString((Long[]) val);

     if (type == float[].class)
     return Arrays.toString((float[]) val);

     if (type == Float[].class)
     return Arrays.toString((Float[]) val);

     if (type == double[].class)
     return Arrays.toString((double[]) val);

     if (type == Double[].class)
     return Arrays.toString((Double[]) val);

     if (type == char[].class)
     return Arrays.toString((char[]) val);

     if (type == Character[].class)
     return Arrays.toString((Character[]) val);

     if (type == String[].class)
     return Arrays.toString((String[]) val);

     return val != null ? val.toString() : null;
     }
     */
    /**
     * Czy dany typo może być null-em. W przypadku tablicy - czy elementy talicy
     * mogą być null-ami
     */
    public static void checkStaticMethodInvoke(Method method, Class<?>... parameters) {
        int mods = method.getModifiers();

        if (parameters == null)
            parameters = new Class<?>[0];

        String name = method.getDeclaringClass().getName() + "." + method.getName();

        if (!Modifier.isStatic(mods))
            throw new RuntimeException(LUtil.METHOD_ISNT_STATIC.toString(name));

        if (Modifier.isAbstract(mods))
            throw new RuntimeException(LUtil.METHOD_IS_ABSTRACT.toString(name));

        if (true)
            return;

        //todo: dodac obsluge opcjonalnychj parametrow metody
        Class<?>[] params = method.getParameterTypes();

        boolean ok = params.length == parameters.length;

        if (ok)
            for (int i = 0; i < parameters.length; i++)
                if (!parameters[0].equals(params[0])) {
                    ok = false;
                    break;
                }

        if (ok)
            return;

        if (parameters.length == 0)
            throw new RuntimeException(LUtil.METHOD_CANT_TEKE_ARGS.toString(name));

        Strings list = new Strings();
        for (Class<?> cls : parameters)
            list.add(cls.getName());

        throw new RuntimeException(LUtil.METHOD_MUST_TAKE_ARGS.toString(name));

    }

    @SuppressWarnings("unchecked")
    public static <T> T getObject(Object object, String... fieldName) {
        try {
            Object obj = object;

            for (String s : fieldName) {
                Class<?> cls = obj instanceof Class ? (Class<?>) obj
                        : obj.getClass();
                Field field = cls.getDeclaredField(s);
                field.setAccessible(true);
                obj = field.get(obj);
            }

            return (T) obj;
        } catch (Throwable e) {
            Log.warning(e);
            return null;
        }

    }

    /**
     * Zwraca metodę danej klasy, umożliwia wykonaie. Aby wykonać należy wywołać
     * metodę invoke()
     *
     * @param clazz
     * @param name
     * @param args
     * @return
     */
    public static Method getMethod(Class<?> clazz, String name, Class<?>... args) {
        try {
            Method m = clazz.getDeclaredMethod(name, args);
            m.setAccessible(true);
            return m;
        } catch (Exception ex) {
            throw new ThrowableException(ex);
        }
    }

    public static Method getMethod(String fullName, Class<?>... args) {
        try {
            return getMethod(Class.forName(fullName.substring(0, fullName.lastIndexOf(".")),
                    false, ClassLoader.getSystemClassLoader()),
                    fullName.substring(fullName.lastIndexOf(".") + 1), args);
        } catch (Exception ex) {
            throw new ThrowableException(ex);
        }
    }

    public static Object getFieldValueF(Object instance, String fieldName) throws NoSuchFieldException {
        try {
            return getField(instance.getClass(), fieldName).get(instance);
        } catch (Exception ex) {
            throw new NoSuchFieldException(fieldName);
        }
    }

    public static Object getFieldValue(Object instance, String fieldName) {
        try {
            return getField(instance.getClass(), fieldName).get(instance);
        } catch (Exception ex) {
            Log.warning(ex);
            return null;
        }
    }

    public static Field getField(Class<?> clazz, String fieldName) {
        try {
            Field field = clazz.getDeclaredField(fieldName);
            field.setAccessible(true);
            return field;
        } catch (Exception ex) {
            throw new ThrowableException(ex);
        }
    }

    public static Field getFieldF(String fullName) throws NoSuchFieldException {
        try {
            Class<?> cls = Class.forName(fullName.substring(0, fullName.lastIndexOf(".")),
                    false, ClassLoader.getSystemClassLoader());
            Field field = cls.getDeclaredField(fullName.substring(fullName.lastIndexOf(".") + 1));
            field.setAccessible(true);
            return field;
        } catch (Exception ex) {
            throw new NoSuchFieldException(fullName);
        }
    }

    public static Field getField(String fullName) throws NoSuchFieldException {
        return getFieldF(fullName);
    }

    public static Class<?> getClassGenericType(Class<?> clazz, int index) {
        Class<?>[] arr = getClassGenericTypes(clazz);
        return index >= 0 && index < arr.length ? arr[index] : null;
    }

    /**
     * Zwraca listę typów generycznych danej klasy, np Map<Integer, String>
     * zwróci [Integer, String]
     *
     * @param clazz
     * @return
     */
    @Deprecated
    public static Class<?>[] getClassGenericTypes(Type type) {

        List<Class<?>> list = new LinkedList<>();

        if (type instanceof ParameterizedType) {
            Type[] types = ((ParameterizedType) type).getActualTypeArguments();

            if (types != null)
                for (Type t : types)
                    if (t instanceof TypeVariable)
                        list.add((Class<?>) ((TypeVariable) t).getGenericDeclaration());
                    else if (t instanceof Class)
                        list.add((Class<?>) t);
        }

        return list.toArray(new Class<?>[0]);
    }

    /**
     * Zwraca pierwszy typ generyczny klasy lub nulla, jesli typ nie istnieje
     *
     * @param clazz
     * @return
     */
    public static Class<?> getClassGenericType(Type clazz) {
        Class<?>[] classes = getClassGenericTypes(clazz);
        return classes.length > 0 ? classes[0] : null;
    }

    public static boolean classImplements(Class<?> cls, Class<?> interfaceClass) {

        if (cls == null || interfaceClass == null)
            return false;

        if (!interfaceClass.isInterface())
            return false;

        if (cls.isInterface() && cls.equals(interfaceClass))
            return true;

        while (cls != null) {
            Class[] interfaces = cls.getInterfaces();

            for (Class iface : interfaces)
                if (iface.equals(interfaceClass))
                    return true;
            cls = cls.getSuperclass();
        }
        return false;
    }

    /**
     * Zwraca listę pól danej klasy wraz z klasami nadzędnymi
     *
     * @param cls
     */
    public static Field[] getDeclaredFields(Class<?> cls) {
        List<Field> list = new LinkedList<>();

        while (cls != null) {
            list.addAll(Arrays.asList(cls.getDeclaredFields()));
            cls = cls.getSuperclass();
        }
        return list.toArray(new Field[list.size()]);

    }

    /**
     * Zwraca adnotację danej metody z uwzglednieniem metod macierzystych.
     * Przykładowo jeśli adnotacja zadeklarowana jest na poziomie metody
     * abstrakcyjnej lub samego interfejsu, metoda zwróci jej deklarację.
     *
     * Funkcja rekurencyjnie przeszukuje kolejne klasy macierzyste pod kątem
     * wystepowania przeciążonej metody. W momencie gdy znajdzie metodę o
     * identycznej nazwie oraz parametrach wtedy weryfikowana jest deklaracja
     * danej adnotacji.
     *
     * @param <T>
     * @param method
     * @param annotationClass
     * @return
     */
    public static <T extends Annotation> T getAnnotation(Method method, Class<T> annotationClass) {

        T ann = method.getAnnotation(annotationClass);
        if (ann != null)
            return ann;

        Class<?> superClass = method.getDeclaringClass().getSuperclass();
        if (superClass != null)
            for (Method m : superClass.getDeclaredMethods())
                if (m.getName().equals(method.getName())
                        && Arrays.equals(m.getParameters(), method.getParameters()))
                    return getAnnotation(m, annotationClass);
        return null;
    }

    /**
     * Zwraca listę metod danej klasy oraz wszystkich klas macierzystych
     *
     * @param cls
     * @param accesibleOnly
     * @return
     */
    public static List<Method> getMethods(Class<?> cls, boolean accesibleOnly) {

        List<Method> list = new LinkedList<>();
        while (cls != null) {
            for (Method m : accesibleOnly ? cls.getMethods()
                    : cls.getDeclaredMethods())
                list.add(m);
            cls = cls.getSuperclass();
        }
        return list;
    }

    /**
     * Zwraca mapę metod, które posiadają daną deklaracje. Analizowana jest
     * bieżąca klasa wraz ze wszystkimi macierzystymi
     *
     * @param <T>
     * @param baseClass
     * @param accesibleOnly
     * @param annotationClass
     * @return
     */
    public static <T extends Annotation> Map<Method, T> getMethods(final Class<?> baseClass,
            boolean accesibleOnly, Class<T> annotationClass) {

        Map<Method, T> map = new LinkedHashMap<>();

        Class<?> cls = baseClass;

        while (cls != null) {
            for (Method m : accesibleOnly ? cls.getMethods()
                    : cls.getDeclaredMethods()) {

                if (cls != baseClass && Modifier.isPrivate(m.getModifiers()))
                    continue;

                T ann = m.getAnnotation(annotationClass);
                if (ann != null)
                    map.put(m, ann);
            }
            cls = cls.getSuperclass();
        }

        return map;
    }

    /**
     * Czy dany modyfikator zawiera wszystkie wymagane
     *
     * @param modifier
     * @param modifiers
     * @return
     */
    public static boolean hasModifires(int modifier, int... modifiers) {
        if (modifiers != null)
            for (int mod : modifiers)
                if ((mod & modifier) == 0)
                    return false;
        return true;
    }

    /**
     * Pobierz ścieżkę pliku jar na podstawie klasy
     *
     * @param cls
     * @return
     */
    public static File getJarPath(Class<?> cls) {
        try {
            return new File(cls.getProtectionDomain().getCodeSource().getLocation().toURI());
        } catch (Exception e) {
            return new File(cls.getProtectionDomain().getCodeSource().getLocation().getPath());
        }
    }

    /**
     * Zwraca ścieżkę gdzie znajdują się pliki klas na postawie nazwy klasy
     *
     * @param cls
     * @return
     * @throws URISyntaxException
     */
    public static Path getClassPath(Class<?> cls) throws URISyntaxException {
        File clsPath = getJarPath(cls);
        if (clsPath.getName().toLowerCase().endsWith(".class")) {
            // można zapisać tak jak poniżej, ale to nie zadziała np na GlassFishu
            // clsPath = new File(AppContext.class.getClassLoader().getResource("").toURI());
            String p1 = "/" + cls.getName().replace(".", "/") + ".class";
            String p = clsPath.toString().replace("\\", "/");
            if (p.endsWith(p))
                clsPath = new File(p.substring(0, p.length() - p1.length()));
        }
        return new Path(clsPath);
    }

    /**
     * Weryfikuje, czy klasa ma zadeklarowany konstruktor o danych parametrach.
     * Jesli nie, zwracany jest wyjątek
     *
     * @param cls
     * @param parameterTypes
     */
    public static void checkConstructor(Class<?> cls, Class<?>... parameterTypes) {
        try {
            cls.getConstructor(parameterTypes);

        } catch (Throwable ex) {
            Strings strs = new Strings();
            if (parameterTypes != null)
                for (Class c : parameterTypes)
                    if (c != null)
                        strs.add(c.getSimpleName());

            throw new CoreException(LUtil.LACK_OF_CTOR_WITH_ARGS.toString(cls.getName(), strs.toString(", ")), ex);

        }
    }

    /**
     * Werufikowane sa modyfikatory danej metody. Jesli modyfikator zostanie
     * zadeklarowany jako ujemny (np -Modifier.PUBLIC), oznacza, że modyfikator
     * nie może wystąpić w metodzie
     *
     * @param m
     * @param modifiers
     */
    static void checkModifiers(String name, int mods, int... modifiers) throws CoreException {
        if (modifiers == null || modifiers.length == 0)
            return;

        for (int mod : modifiers) {

            boolean exclude = mod < 0;
            if (exclude)
                mod *= -1;

            if (!exclude && (mod & mods) == 0)
                throw new CoreException(name + " " + LUtil.MUST_HAVE_MODIFIER.toString() + " "
                        + Modifier.toString(mod));

            if (exclude && (mod & mods) != 0)
                throw new CoreException(name + " " + LUtil.CANT_HAVE_MODIFIER.toString() + " "
                        + Modifier.toString(mod));

        }
    }

    public static void checkModifiers(Method m, int... modifiers) throws CoreException {
        checkModifiers(LUtil.METHOD.toString() + m.getDeclaringClass().getName()
                + "." + m.getName(), m.getModifiers(), modifiers);
    }

    public static String getClassHash(Class<?> cls) {
        if (cls == null)
            return null;

        String name = cls.getName();
        if (name.contains("$$Lambda$") && name.contains("/"))
            name = name.substring(0, name.indexOf("/"));

        return Hashes.idHash8(name);

    }
}
