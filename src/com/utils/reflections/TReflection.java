package com.utils.reflections;

import com.exceptions.CoreException;
import com.utils.collections.TList;
import java.lang.annotation.Annotation;
import java.lang.reflect.Modifier;
import java.lang.reflect.ParameterizedType;
import java.lang.reflect.Type;
import java.lang.reflect.TypeVariable;

/**
 * @author Miłosz Ziernik
 * @date 16 grudnia 2015
 * @encoding UTF-8
 */
public interface TReflection {

    public int getModifiers();

    default boolean isFinal() {
        return Modifier.isFinal(getModifiers());
    }

    default boolean isAbstract() {
        return Modifier.isAbstract(getModifiers());
    }

    default boolean isPrivate() {
        return Modifier.isPrivate(getModifiers());
    }

    default boolean isStatic() {
        return Modifier.isStatic(getModifiers());
    }

    default boolean isPublic() {
        return Modifier.isPublic(getModifiers());
    }

    default boolean isProtected() {
        return Modifier.isProtected(getModifiers());
    }

    public void checkModifiers(int... modifiers) throws CoreException;

    public Class<?> getDeclaringClass();

    public String getFullName();

    public <A extends Annotation> A getAnnotation(Class<A> annotationClass);

    public String getName();

    public Class<?> getReturnType();

    public Object invoke(Object parent, Object... args);

    static Class<?>[] getClassGenericTypes(Type[] genericInterfaces) {
        TList<Class<?>> list = new TList<>();
        for (Type tt : genericInterfaces)
            if (tt instanceof ParameterizedType) {
                Type[] types = ((ParameterizedType) tt).getActualTypeArguments();

                if (types != null)
                    for (Type t : types)
                        if (t instanceof TypeVariable)
                            list.add((Class<?>) ((TypeVariable) t).getGenericDeclaration());
                        else if (t instanceof Class)
                            list.add((Class<?>) t);
            }

        return list.toArray(new Class<?>[0]);
    }
}
