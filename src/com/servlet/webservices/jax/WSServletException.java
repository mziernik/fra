package com.servlet.webservices.jax;

import com.sun.xml.ws.util.exception.JAXWSExceptionBase;

/**
 *
 * @author WS Development Team
 */
final class WSServletException extends JAXWSExceptionBase {

    public WSServletException(String key, Object... args) {
        super(key, args);
    }

    public WSServletException(Throwable throwable) {
        super(throwable);
    }

    public String getDefaultResourceBundleName() {
        return "com.sun.xml.ws.resources.wsservlet";
    }
}
