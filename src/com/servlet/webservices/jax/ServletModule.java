package com.servlet.webservices.jax;

import com.sun.istack.NotNull;
import com.sun.xml.ws.api.server.Module;
import com.sun.xml.ws.api.server.WebModule;
import javax.servlet.http.HttpServletRequest;

/**
 * {@link WebModule} that is a servlet container.
 *
 * @see WebModule
 * @see Module
 *
 * @author Jitendra Kotamraju
 */
public abstract class ServletModule extends WebModule {

    /**
     * Gets the host, port, and context path portion of this module using
     * {@link HttpServletRequest}
     *
     * <p>
     * This method follows the convention of the
     * <tt>HttpServletRequest.getContextPath()</tt>, and accepts strings like
     * "http://myhost" (for web applications that are deployed to the root
     * context path), or "http://myhost/foobar" (for web applications that are
     * deployed to context path "/foobar")
     *
     */
    public @NotNull
    String getContextPath(HttpServletRequest req) {
        return ServletConnectionImpl.getBaseAddress(req);
    }

}
