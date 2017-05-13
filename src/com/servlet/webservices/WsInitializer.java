package com.servlet.webservices;

import com.context.index.Index;
import com.exceptions.CoreException;
import com.lang.LServlet;

public class WsInitializer {

    public final static boolean hasLibrary;

    public static void initializeWebServices() throws Exception {

        if (!Index.webServices.isEmpty() && !hasLibrary)
            throw new CoreException(LServlet.LIB_NOT_FOUND.toString("webservices-rt-2.3.jar"),
                    LServlet.CLASS_NOT_FOUND.toString("com.sun.xml.ws.api.WSBinding"));

        if (hasLibrary)
            WebServices.initializeWebServices();
    }

    public static void contextDestroyed() {
        if (hasLibrary)
            WebServices.contextDestroyed();
    }

    static {
        boolean b;
        try {
            Class.forName("com.sun.xml.ws.api.WSBinding");
            b = true;
        } catch (ClassNotFoundException ex) {
            b = false;
        }
        hasLibrary = b;
    }

}
