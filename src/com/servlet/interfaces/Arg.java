package com.servlet.interfaces;

import com.config.CService;
import com.exceptions.CoreException;
import com.exceptions.ServiceException;
import com.utils.Utils;
import com.utils.Is;
import com.utils.collections.Strings;
import com.utils.collections.TList;
import com.utils.reflections.*;
import java.lang.annotation.*;
import java.lang.reflect.*;
import java.util.*;

@Inherited
@Target(value = {ElementType.TYPE, ElementType.PARAMETER, ElementType.FIELD})
@Retention(value = RetentionPolicy.RUNTIME)
public @interface Arg {

    public String name(); //nazwa argumentu

    public boolean nonEmpty() default true;

    public boolean required() default true;

    public String def() default DEF_EMPTY; // wartość domyślna

    public final static String DEF_EMPTY = "{CC45B3C5-3479-4536-A5BB-9ED0A29A748}";

    public static class ArgMeta {

        public final Field field;
        public final Arg ann;
        public final TClass<?> cls;
        public final Type type;
        public final String shortTypeName;
        public final Method method;
        public final int index;
        public final String name;
        public final boolean required;
        public final boolean nonEmpty;
        public final String defaultValue;

        public final String fullName;
        private Class<?> genericClass;

        public ArgMeta(Field field, Class<?> type, Arg ann) {
            this.ann = ann;
            this.field = field;
            this.cls = new TClass<>(type, field.getGenericType());
            this.nonEmpty = ann.required() && ann.nonEmpty();
            this.required = nonEmpty || ann.required();
            this.defaultValue = Arg.DEF_EMPTY.equals(ann.def()) ? null : ann.def();
            this.type = field.getGenericType();
            this.name = ann.name();
            method = null;
            index = -1;
            this.fullName = field.getDeclaringClass().getName() + "." + field.getName();
            this.shortTypeName = getShortTypeName(this.type.getTypeName());
            verify();
        }

        public ArgMeta(Method method, Parameter param, int index) {
            this.ann = param.getAnnotation(Arg.class);
            this.nonEmpty = ann != null ? ann.required() && ann.nonEmpty() : true;
            this.required = nonEmpty || (ann != null ? ann.required() : true);
            this.defaultValue = ann == null || Arg.DEF_EMPTY.equals(ann.def()) ? null : ann.def();
            this.field = null;
            this.cls = new TClass<>(param.getType(), param.getParameterizedType());
            this.type = param.getParameterizedType();
            this.method = method;
            this.fullName = method.getDeclaringClass().getName() + "." + method.getName();
            this.name = ann != null ? ann.name() : cls.raw.getSimpleName().toLowerCase() + (index + 1);
            this.index = index;
            this.shortTypeName = getShortTypeName(this.type.getTypeName());
            if (ann != null)
                verify();
        }

        private static String getShortTypeName(String tname) {

            Strings list = new Strings();
            for (String s : tname.replace("<", ",").replace(">", ",").split("\\,")) {
                s = s.trim();
                if (s.isEmpty())
                    continue;
                if (s.contains("."))
                    s = s.substring(s.lastIndexOf(".") + 1);
                list.add(s);
            }

            String result = list.first(true);

            if (!list.isEmpty())
                result += "<" + list.toString(", ") + ">";

            return result;
        }

        private void verify() {

            if (cls.isPrimitive())
                throw new CoreException((field != null
                        ? "Pole [" + cls.raw.getSimpleName() + "] " + fullName
                        : "Argument [" + cls.raw.getSimpleName() + "] metody " + fullName)
                        + " nie może być typu prymitywnego");

            if (!TypeAdapter.isSupporterd(cls.raw))
                throw new CoreException((field != null
                        ? "Pole [" + cls.raw.getSimpleName() + "] " + fullName
                        : "Argument [" + cls.raw.getSimpleName() + "] metody " + fullName)
                        + " nie może być zdeserializowany(e)");

            if (!cls.instanceOf(Collection.class)
                    && !cls.raw.isArray()
                    && field != null
                    && new TField(field).isFinal()
                    && cls.isSimple())
                throw new CoreException(
                        "Pole [" + cls.raw.getSimpleName() + "] " + fullName
                        + " nie może być finalne");
        }

        private String methodName(String methodName) {
            return ((Is.empty(methodName) ? "" : "\"" + methodName + "\"")
                    + (CService.devMode() ? " (" + fullName + ")" : "")).trim();
        }

        public Object toObject(Collection<String> argList, String methodName, Object instance) {
            TList<String> list = new TList<>(argList);

            if (required && list.isEmpty() && defaultValue != null)
                list.add(defaultValue);

            if (required && list.isEmpty())
                throw new ServiceException(String.format("Brak argumentu \"%s\" metody %s",
                        name, methodName(methodName)));

            if (nonEmpty && (list.isEmpty()
                    || (!cls.instanceOf(Collection.class)
                    && !cls.raw.isArray()
                    && Is.empty(new TList<>(argList).first()))))
                throw new ServiceException(String.format("Wartość argumentu \"%s\" metody %s nie może być pusta",
                        name, methodName(methodName)));

            Object result;
            try {
                result = cls.deserialize(list, instance);
            } catch (Throwable e) {
                throw new ServiceException(String.format("Nieprawidłowa wartość (\"%s\") argumentu \"%s\" metody %s",
                        list.toString(", "), name, methodName(methodName)));
            }

            if (result == null && required)
                throw new ServiceException(String.format(
                        "Argument \"%s\" metody %s nie może być null-em",
                        name, methodName(methodName)));
            return result;
        }

    }

}
