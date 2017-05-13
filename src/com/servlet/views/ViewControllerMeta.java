package com.servlet.views;

import com.utils.reflections.Reflections;
import com.context.index.Index;
import com.servlet.interfaces.JsMethod;
import java.lang.reflect.Method;
import java.lang.reflect.Modifier;
import java.util.LinkedList;

/**
 * @author Miłosz Ziernik
 * @date 27 października 2015
 * @encoding UTF-8
 */
public class ViewControllerMeta {

    public Class<? extends ViewController> view;
    public final String id;
    public final LinkedList<JsMethod.JsMethodImpl> jsMethods = new LinkedList<>();

    public ViewControllerMeta(Class<? extends ViewController> view) {
        this.view = view;
        id = getId(view);

        for (Method m : view.getDeclaredMethods()) {
            JsMethod jsMethod = m.getAnnotation(JsMethod.class);
            if (jsMethod == null)
                continue;
            Reflections.checkModifiers(m, Modifier.PUBLIC, -Modifier.ABSTRACT, -Modifier.STATIC);
            jsMethods.add(new JsMethod.JsMethodImpl(jsMethod, m));
        }
    }

    public static ViewControllerMeta get(String id) {
        for (ViewControllerMeta meta : Index.views)
            if (meta.id.equals(id))
                return meta;
        return null;
    }

    public static ViewControllerMeta get(Class<? extends ViewController> view) {
        for (ViewControllerMeta meta : Index.views)
            if (meta.view.equals(view))
                return meta;
        return null;
    }

    public static String getId(Class<? extends ViewController> cls) {
        return Reflections.getClassHash(cls);
    }

}
