package com.servlet.controller;

import com.context.index.Index;
import java.util.*;

public class Controllers {
    //ToDo: rozbudować

    /**
     * Zwraca kontrolery należące do danego pakietu
     *
     * @param pckg
     * @param includeChildren
     * @return
     */
    public static LinkedList<ControllerMetaData> getByPackage(Package pckg, boolean includeChildren) {
        LinkedList<ControllerMetaData> list = new LinkedList<>();
        String name = pckg.getName();
        for (ControllerMetaData meta : Index.controllers)
            if ((includeChildren && meta.controller.getPackage().getName().startsWith(name))
                    || (!includeChildren && meta.controller.getPackage().getName().equals(name)))
                list.add(meta);
        return list;
    }

}
