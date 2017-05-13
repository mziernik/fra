package org.apache.juli.logging;

import java.util.Properties;
import java.util.logging.LogManager;

public /* abstract */ class LogFactory {

    // ----------------------------------------------------- Manifest Constants
    /**
     * The name of the property used to identify the LogFactory implementation
     * class name.
     */
    public static final String FACTORY_PROPERTY
            = "org.apache.commons.logging.LogFactory";

    /**
     * The fully qualified class name of the fallback <code>LogFactory</code>
     * implementation class to use, if no other can be found.
     */
    public static final String FACTORY_DEFAULT
            = "org.apache.commons.logging.impl.LogFactoryImpl";

    /**
     * The name of the properties file to search for.
     */
    public static final String FACTORY_PROPERTIES
            = "commons-logging.properties";

    public static final String HASHTABLE_IMPLEMENTATION_PROPERTY
            = "org.apache.commons.logging.LogFactory.HashtableImpl";

    private static LogFactory singleton = new LogFactory();

    Properties logConfig;

    // ----------------------------------------------------------- Constructors
    /**
     * Protected constructor that is not available for public use.
     */
    private LogFactory() {
        logConfig = new Properties();
    }

    // hook for syserr logger - class level
    void setLogConfig(Properties p) {
        this.logConfig = p;
    }

    public Log getInstance(String name) {
        return DirectJDKLog.getInstance(name);
    }

    public void release() {
        DirectJDKLog.release();
    }

    public Object getAttribute(String name) {
        return logConfig.get(name);
    }

    public String[] getAttributeNames() {
        String result[] = new String[logConfig.size()];
        return logConfig.keySet().toArray(result);
    }

    public void removeAttribute(String name) {
        logConfig.remove(name);
    }

    public void setAttribute(String name, Object value) {
        logConfig.put(name, value);
    }

    public Log getInstance(Class<?> clazz) {
        return getInstance(clazz.getName());
    }

    public static LogFactory getFactory() {
        return singleton;
    }

    public static Log getLog(Class<?> clazz) {
        return (getFactory().getInstance(clazz));

    }

    public static Log getLog(String name) {
        return (getFactory().getInstance(name));

    }

    public static void release(ClassLoader classLoader) {
        // JULI's log manager looks at the current classLoader so there is no
        // need to use the passed in classLoader, the default implementation
        // does not so calling reset in that case will break things
        if (!LogManager.getLogManager().getClass().getName().equals(
                "java.util.logging.LogManager"))
            LogManager.getLogManager().reset();
    }

    public static void releaseAll() {
        singleton.release();
    }

    public static String objectId(Object o) {
        if (o == null)
            return "null";
        else
            return o.getClass().getName() + "@" + System.identityHashCode(o);
    }
}
