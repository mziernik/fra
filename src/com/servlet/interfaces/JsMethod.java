package com.servlet.interfaces;

import com.exceptions.CoreException;
import com.utils.reflections.TMethod;
import java.lang.annotation.*;
import java.lang.reflect.*;

/**
 * Adnotacja umożliwiająca wywołanie metody Javy z poziomu JavaScript-u
 * Adrnotacja ta działa w klasach dziedziczących po Page oraz View
 *
 * @author Miłosz
 */
@Inherited
@Target(value = {ElementType.METHOD})
@Retention(value = RetentionPolicy.RUNTIME)
public @interface JsMethod {

    public String name() default ""; // nazwa metody

    public static class JsMethodImpl {

        public final String name;
        public final TMethod method;
        public final JsMethod intf;

        public JsMethodImpl(JsMethod intf, Method method) {
            this.name = intf.name().trim().isEmpty()
                    ? method.getName() : intf.name();
            this.method = new TMethod(method);
            this.intf = intf;

            for (Arg.ArgMeta meta : this.method.arguments)
                if (meta.ann == null)
                    throw new CoreException("Argument " + meta.name + " metody "
                            + this.method + " musi mieć zadeklarowaną adnotację @Arg");

        }

    }
}
