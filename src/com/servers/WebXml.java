package com.servers;

import com.xml.*;

import com.servlet.MainServlet;
import java.io.File;
import java.io.IOException;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Map.Entry;

public class WebXml extends XML {

    public final File file;

    public final Map<Class, String> listenners = new LinkedHashMap<>();
    public final Map<Class, String> filters = new LinkedHashMap<>();

    public WebXml(File webAppPath) throws XmlException {
        super(SOURCE);

        file = new File(webAppPath, "WEB-INF/web.xml");
        if (file.exists())
            load(file);
    }

    public void verify() throws IOException, XmlException {

        // weryfikacja czy isttnieje deklaracja serwletu com.servlet.MainServlet
        XmlNode servlet = null;
        for (XmlNode node : getNodes("servlet")) {
            XmlNode ncls = node.node("servlet-class");
            if (ncls != null && "com.servlet.MainServlet".equals(ncls.getText())) {
                servlet = node;
                break;
            }
        }

        if (servlet == null) {
            servlet = addNode("servlet");
            servlet.nodeC("servlet-class").setText(MainServlet.class.getName());
            servlet.nodeC("servlet-name").setText("MainServlet");
            servlet.nodeC("async-supported").setText("true");
            servlet.nodeC("load-on-startup").setText("0");

            XmlNode multipart = servlet.nodeC("multipart-config");
            multipart.nodeC("file-size-threshold").setText(Integer.toString(1024 * 1024));
            //multipart.nodeC("location");
            //multipart.nodeC("max-file-size");
            // multipart.nodeC("max-request-size");

            XmlNode mapping = addNode("servlet-mapping");
            mapping.nodeC("servlet-name").setText("MainServlet");
            mapping.nodeC("url-pattern").setText("/");

            //-------------------
            /* servlet = nodeAdd("servlet");
             servlet.nodeC("servlet-class").setText(MainWebSocket.class.getName());
             servlet.nodeC("servlet-name").setText("MainWebSocket");

             mapping = nodeAdd("servlet-mapping");
             mapping.nodeC("servlet-name").setText("MainWebSocket");
             mapping.nodeC("url-pattern").setText("/{*}");
             */
            for (Entry<Class, String> en : listenners.entrySet()) {

                XmlNode listenner = addNode("listener");
                listenner.nodeC("description").setText(en.getValue());
                listenner.nodeC("listener-class").setText(en.getKey().getName());
                /* <listener>
                 <description>sdfsdsdgsd</description>
                 <listener-class>listenner.class</listener-class>
                 </listener> */
            }

            for (Entry<Class, String> en : filters.entrySet()) {

                XmlNode filter = addNode("filter");
                filter.nodeC("filter-name").setText(en.getValue());
                filter.nodeC("filter-class").setText(en.getKey().getName());
                filter.nodeC("filter-description").setText(en.getKey().getName());

                /* <listener>
                 <description>sdfsdsdgsd</description>
                 <listener-class>listenner.class</listener-class>
                 </listener> */
            }

            this.save(file);
        }

    }

    private final static String SOURCE = "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
            + "<web-app version=\"3.0\" xmlns=\"http://java.sun.com/xml/ns/javaee\" \n"
            + "         xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" \n"
            + "         xsi:schemaLocation=\"http://java.sun.com/xml/ns/javaee http://java.sun.com/xml/ns/javaee/web-app_3_0.xsd\">\n"
            + "</web-app>";

}


/*
 <?xml version="1.0" encoding="UTF-8"?>
 <web-app version="3.0" xmlns="http://java.sun.com/xml/ns/javaee" xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:schemaLocation="http://java.sun.com/xml/ns/javaee http://java.sun.com/xml/ns/javaee/web-app_3_0.xsd">
 <context-param>
 <description>sdf</description>
 <param-name>param1</param-name>
 <param-value>value</param-value>
 </context-param>
 <filter>
 <description>sdfsdfsdfsdg</description>
 <filter-name>Filtr1</filter-name>
 <filter-class>class</filter-class>
 </filter>
 <listener>
 <description>sdfsdsdgsd</description>
 <listener-class>listenner.class</listener-class>
 </listener>
 <servlet>
 <servlet-name>NewServlet</servlet-name>
 <servlet-class>aaa.NewServlet</servlet-class>


 </servlet>
 <servlet-mapping>
 <servlet-name>NewServlet</servlet-name>
 <url-pattern>/</url-pattern>
 </servlet-mapping>
 <session-config>
 <session-timeout>
 30
 </session-timeout>
 </session-config>
 </web-app>

 */
