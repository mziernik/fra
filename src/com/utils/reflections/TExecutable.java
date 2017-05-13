package com.utils.reflections;

import com.exceptions.EError;
import com.exceptions.ServiceException;
import com.lang.LUtil;
import com.utils.collections.Strings;
import java.lang.reflect.*;
import java.util.Arrays;
import java.util.LinkedList;

/**
 * @author Miłosz Ziernik
 * @date 16 grudnia 2015
 * @encoding UTF-8
 */
public class TExecutable {

    private final Executable[] execs;
    private final Object instance;
    private final Class<?> parent;

    private boolean constructor;

    public TExecutable(Object instance, Class<?> cls) {
        this(instance, cls.getDeclaredConstructors());
    }

    public TExecutable(Object instance, Executable... execs) {
        this.execs = execs;
        this.instance = instance;
        Class<?> parent = null;

        for (Executable e : execs) {

            if (parent == null)
                parent = e.getDeclaringClass();

            if (parent != e.getDeclaringClass())
                throw new IllegalArgumentException();

            constructor = (e instanceof Constructor);

            if (e instanceof Constructor && !Boolean.TRUE.equals(constructor))
                throw new IllegalArgumentException();

            if (e instanceof Method && !Boolean.FALSE.equals(constructor))
                throw new IllegalArgumentException();
        }
        if (parent == null)
            throw new IllegalArgumentException();

        this.parent = parent;
    }

    private Object doInvoke(Executable ex, Object[] args) {
        try {
            ex.setAccessible(true);

            if (ex instanceof Constructor)
                return ((Constructor) ex).newInstance(args);

            if (ex instanceof Method)
                return ((Method) ex).invoke(instance, args);

        } catch (Throwable e) {

            if (e instanceof InvocationTargetException)
                e = e.getCause();

            EError.addDetails(e, LUtil.METHOD.toString(), ex.getName());

            if (e instanceof Error)
                throw (Error) e;

            if (e instanceof RuntimeException)
                throw (RuntimeException) e;

            throw new ServiceException(e);
        }
        return null;
    }

    private boolean sameType(Class<?> cls, Object o, boolean includeNull) {

        if (!includeNull && o == null)
            return false;

        if (cls.isPrimitive() && o == null)
            return false;

        if (includeNull && o == null)
            return true;

        return new TClass(o.getClass()).instanceOf(cls);
    }

    public Object invoke(Object... args) {

        if (args == null)
            args = new Object[0];

        // jesli klasa nie jest statyczna, jako pierwszy parametr przyjmuje instancję rodzica
        if (constructor && !new TClass(parent).isStatic()
                && parent.getDeclaringClass() != null) {
            Object[] result = new Object[args.length + 1];
            result[0] = instance;
            for (int i = 0; i < args.length; i++)
                result[i + 1] = args[i];
            args = result;
        }

        try {
            // warinat 1: parametry metody pokrywają się 1:1 z parametrami konstruktora
            for (Executable ex : execs) {
                Parameter[] params = ex.getParameters();

                if (args.length != params.length)
                    continue;

                boolean ok = true;

                for (int i = 0; i < params.length; i++)
                    ok &= sameType(params[i].getType(), args[i], true);

                if (ok)
                    return doInvoke(ex, args);
            }

            LinkedList<Object> pars = new LinkedList<>();
            pars.addAll(Arrays.asList(args));

            // warinat 2: dopasuj parametry, brakujące uzupełnij nullami
            for (Executable ex : execs) {
                Parameter[] params = ex.getParameters();
                Object[] cpars = new Object[params.length];

                for (int i = 0; i < params.length; i++) {

                    Class<?> type = params[i].getType();

                    for (Object o : pars)
                        if (o != null && new TClass(o.getClass()).instanceOf(type)) {
                            cpars[i] = o;
                            pars.remove(o);
                            break;
                        }

                    // wymagany parametr jest typu prymitywnego a deklarowany jest nullem
                    if (type.isPrimitive() && cpars[i] == null) {
                        cpars = null;
                        break;
                    }
                }
                if (cpars != null)
                    return doInvoke(ex, cpars);
            }

        } catch (Error | RuntimeException e) {
            throw e;
        } catch (Throwable e) {
            throw new ServiceException(e);
        }

        Strings lst = new Strings();

        for (Object o : args)
            lst.add(o != null ? o.getClass().getSimpleName() : "null");

        Strings lst2 = new Strings();

        for (Executable ex : execs) {
            Parameter[] params = ex.getParameters();
            Strings str = new Strings();
            for (Class c : ex.getParameterTypes())
                str.add(c.getSimpleName());
            lst2.add(str.toString(", "));
        }

        if (lst2.size() > 1)
            lst2.prefix("(").sufix(")");

        RuntimeException ex = new RuntimeException(LUtil.ARGS_CONFLICT.toString());
        if (parent != null)
            EError.addDetails(ex, LUtil.CLASS.toString(), parent.getName());

        EError.addDetails(ex,
                LUtil.ARGS.toString(),
                LUtil.DECLARED_OR_CURRENT.toString(lst2.toString(" lub "), lst.toString(", ")));

        throw ex;
    }

}
