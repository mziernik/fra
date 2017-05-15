package com.utils.reflections;

import com.context.AppContext;
import com.exceptions.CoreException;
import com.exceptions.ThrowableException;
import com.intf.callable.Callable1;
import com.intf.callable.Callable2;
import com.json.JArray;
import com.json.JElement;
import com.json.JObject;
import com.lang.LUtil;
import com.servlet.interfaces.Arg;
import com.servlet.interfaces.Arg.ArgMeta;
import com.utils.collections.Strings;
import com.utils.collections.TList;
import java.lang.annotation.Annotation;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.lang.reflect.Parameter;
import java.util.LinkedList;
import java.util.List;

/**
 * @author Miłosz Ziernik
 * @date 15 grudnia 2015
 * @encoding UTF-8
 */
public class TMethod implements TReflection {

    public final Method raw;
    public final ArgMeta arguments[];

    public TMethod(String fullName) throws ClassNotFoundException, NoSuchMethodException {
        this(getMethod(fullName));
    }

    public TMethod(Method method) {
        this.raw = method;
        int idx = 0;
        List<ArgMeta> args = new LinkedList<>();
        for (Parameter p : method.getParameters())
            args.add(new Arg.ArgMeta(method, p, idx++));

        ArgMeta[] arguments = new Arg.ArgMeta[args.size()];
        args.toArray(arguments);
        this.arguments = arguments;
    }

    private static Method getMethod(String fullName) {
        try {
            Class<?> cls = Class.forName(fullName.substring(0, fullName.lastIndexOf(".")),
                    false, ClassLoader.getSystemClassLoader());
            String name = fullName.substring(fullName.lastIndexOf(".") + 1);

            for (Method m : cls.getDeclaredMethods())
                if (m.getParameterCount() == 0 && m.getName().equals(name))
                    return m;

            for (Method m : cls.getDeclaredMethods())
                if (m.getName().equals(name))
                    return m;

            return null;
        } catch (ClassNotFoundException | SecurityException e) {
            throw new ThrowableException(e);
        }
    }

    public Strings getArgumentNames() {
        Strings lst = new Strings();
        for (ArgMeta arg : arguments)
            lst.add(arg.name);
        return lst;
    }

    @Override
    public String toString() {
        return getFullName();
    }

    public Object invoke(Object parent, JArray params)
            throws IllegalAccessException, IllegalArgumentException, InvocationTargetException {
        raw.setAccessible(true);

        if (raw.getParameterCount() != params.size()) {
            if (!AppContext.releaseMode())
                throw new RuntimeException(LUtil.INCORRECT_ARGS_COUNT.toString());
            return null;
        }

        Object[] args = new Object[params.size()];
        Class<?>[] pTypes = raw.getParameterTypes();

        for (int i = 0; i < params.size(); i++) {
            JElement el = params.element(i);
            if (el.isValue())
                args[i] = new TClass<>(pTypes[i]).deserialize(el.asValue().asString());
        }

        raw.invoke(parent, args);

        return null;
    }

    @Override
    public String getName() {
        return raw.getName();
    }

    @Override
    public <A extends Annotation> A getAnnotation(Class<A> annotationClass) {
        return raw.getAnnotation(annotationClass);
    }

    @Override
    public Object invoke(Object methodObject, Object... args) {
        return new TExecutable(methodObject, raw).invoke(args);
    }

    @Override
    public int getModifiers() {
        return raw.getModifiers();
    }

    @Override
    public void checkModifiers(int... modifiers) throws CoreException {
        Reflections.checkModifiers("Metoda " + raw.getDeclaringClass().getName()
                + "." + raw.getName(), raw.getModifiers(), modifiers);
    }

    @Override
    public String getFullName() {
        return raw.getDeclaringClass().getName() + "." + raw.getName();
    }

    public Object[] createArguments(Callable1<List<Object>, String> argumentsProvider,
            String methodName, Callable2<Object, ArgMeta, Object> postProcess) {
        List<Object> lst = new LinkedList<>();
        for (Arg.ArgMeta arg : arguments) {
            Object obj = null;
            if (arg.ann != null)
                obj = arg.toObject(argumentsProvider.run(arg.name), methodName, null);

            if (postProcess != null)
                obj = postProcess.run(arg, obj);
            lst.add(obj);
        }
        Object[] arr = new Object[lst.size()];
        return lst.toArray(arr);
    }

    public Object[] createArguments(JObject argumentsProvider,
            String methodName, Callable2<Object, ArgMeta, Object> postProcess) {
        List<Object> lst = new LinkedList<>();
        for (Arg.ArgMeta arg : arguments) {
            Object obj = null;
            if (arg.ann != null) {
                Object val = argumentsProvider.getRawValue(arg.name, null);
                TList<Object> list = new TList<Object>();
                list.add(val);
                obj = arg.toObject(list, methodName, null);
            }
            if (postProcess != null)
                obj = postProcess.run(arg, obj);
            lst.add(obj);
        }
        Object[] arr = new Object[lst.size()];
        return lst.toArray(arr);
    }

    @Override
    public Class<?> getDeclaringClass() {
        return raw.getDeclaringClass();
    }

    @Override
    public Class<?> getReturnType() {
        return raw.getReturnType();
    }

}
