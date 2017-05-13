package com.utils.reflections;

import com.exceptions.CoreException;
import com.exceptions.EError;
import com.exceptions.ThrowableException;
import com.lang.LUtil;
import java.lang.annotation.Annotation;
import java.lang.reflect.Field;
import java.util.Arrays;
import java.util.Collection;

/**
 * @author Miłosz Ziernik
 * @date 15 grudnia 2015
 * @encoding UTF-8
 */
public class TField implements TReflection {

    public final Field raw;

    public TField(String field) {

        String fieldName = field;
        String className = null;

        if (!field.contains("."))
            throw new UnsupportedOperationException();

        fieldName = field.substring(field.lastIndexOf(".") + 1);
        className = field.substring(0, field.length() - fieldName.length() - 1);

        try {
            this.raw = new TClass<>(className).raw
                    .getDeclaredField(fieldName);

        } catch (Error | RuntimeException e) {
            throw e;
        } catch (Throwable e) {
            throw new ThrowableException(e);
        }
    }

    public TField(Field field) {
        this.raw = field;
    }

    @Override
    public String toString() {
        return raw.getDeclaringClass().getName() + "." + raw.getName();
    }

    public Object set(Object object, String... values) {
        return set(object, Arrays.asList(values));
    }

    public Object set(Object object, Collection<String> values) {
        try {
            raw.setAccessible(true);

            Object obj = new TClass(raw.getType(), raw.getGenericType())
                    .deserialize(values, raw.get(object));

            if (!isFinal())
                raw.set(object, obj);
            return obj;
        } catch (Throwable ex) {
            throw EError.addDetails(new RuntimeException(ex), "Field", toString());
        }

    }

    /*

     public static void setFieldValue(Field raw, Object obj, String... values)
     throws IllegalArgumentException, IllegalAccessException, ParseException {
     if (raw == null || obj == null)
     return;

     if (values == null || values.length == 0)
     return;

     raw.setAccessible(true);

     Class<?> type = raw.getType();

     Object current = raw.get(obj);

     if (type.isArray()) {
     setFieldArrayValue(raw, obj, values);
     return;
     }

     if (type == Boolean.TYPE)
     raw.setBoolean(obj, Boolean.parseBoolean(values[0]));

     if (type == Boolean.class)
     raw.set(obj, Boolean.parseBoolean(values[0]));

     if (type == Byte.TYPE)
     raw.setByte(obj, Byte.parseByte(values[0]));

     if (type == Byte.class)
     raw.set(obj, Byte.parseByte(values[0]));

     if (type == Short.TYPE)
     raw.setShort(obj, Short.parseShort(values[0]));

     if (type == Short.class)
     raw.set(obj, Short.parseShort(values[0]));

     if (type == Integer.TYPE)
     raw.setInt(obj, Integer.parseInt(values[0]));

     if (type == Integer.class)
     raw.set(obj, Integer.parseInt(values[0]));

     if (type == Long.TYPE)
     raw.setLong(obj, Long.parseLong(values[0]));

     if (type == Long.class)
     raw.set(obj, Long.parseLong(values[0]));

     if (type == Float.TYPE)
     raw.setFloat(obj, Float.parseFloat(sep(values[0])));

     if (type == Float.class)
     raw.set(obj, Float.parseFloat(sep(values[0])));

     if (type == Double.TYPE)
     raw.setDouble(obj, Double.parseDouble(sep(values[0])));

     if (type == Double.class)
     raw.set(obj, Double.parseDouble(sep(values[0])));

     if (type == Character.TYPE) {
     if (values[0].length() != 1)
     throw new ParseException("Nieprawidłowa wartość char", 0);
     raw.setChar(obj, values[0].charAt(0));
     }
     if (type == Character.class) {
     if (values[0].length() != 1)
     throw new ParseException("Nieprawidłowa wartość char", 0);
     raw.set(obj, values[0].charAt(0));
     }
     if (type == String.class)
     raw.set(obj, values[0]);

     }
     */
    @Override
    public int getModifiers() {
        return raw.getModifiers();
    }

    @Override
    public void checkModifiers(int... modifiers) throws CoreException {
        Reflections.checkModifiers(LUtil.FIELD.toString() + " " + raw.getDeclaringClass().getName()
                + "." + raw.getName(), raw.getModifiers(), modifiers);
    }

    @Override
    public String getFullName() {
        return raw.getDeclaringClass().getName() + "." + raw.getName();
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
        return raw.getType();
    }

    @Override
    public Object invoke(Object parent, Object... args) {
        try {
            raw.setAccessible(true);
            return raw.get(parent);
        } catch (IllegalAccessException | IllegalArgumentException ex) {
            throw new ThrowableException(ex);
        }
    }

    public TField setAccessible(boolean b) {
        raw.setAccessible(b);
        return this;
    }

    public Object get(Object parent) throws IllegalArgumentException, IllegalAccessException {
        return raw.get(parent);
    }

}
