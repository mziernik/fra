package com.servlet.views;

import com.utils.Utils;
import com.utils.Is;
import com.utils.reflections.TClass;
import java.util.*;

public class ViewsManager {

    final static int maxInactiveTime = 15000;
    final static LinkedList<ViewController> views = new LinkedList<>();

    public static void processExpired() {

        long now = System.currentTimeMillis();

        LinkedList<ViewController> list;
        synchronized (views) {
            //  list = Utils.asList(views.values());
        }
        /*
        for (ViewController view : list)
            if (view.connection() == null
                    && view.lastPeak + maxInactiveTime < now)
                view.invalidate();
         */
    }

    public static <V extends ViewController> LinkedList<V> get(Class<V> cls) {
        LinkedList<V> list = new LinkedList<>();
        synchronized (ViewsManager.views) {
            for (ViewController ctrl : ViewsManager.views)
                if (cls == null || new TClass<ViewController>(ctrl.getClass()).instanceOf(cls))
                    list.add((V) ctrl);
        }
        return list;
    }

}
