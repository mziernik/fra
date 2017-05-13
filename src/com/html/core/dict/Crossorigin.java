package com.html.core.dict;

/**
 * @author Miłosz Ziernik
 * @date 25 sierpnia 2015
 * @encoding UTF-8
 */
public enum Crossorigin {

    /**
     * A cross-origin request (i.e. with Origin: HTTP header) is performed. But
     * no credential is sent (i.e. no cookie, no X.509 certificate and no HTTP
     * Basic authentication is sent). If the server does not give credentials to
     * the origin site (by not setting the Access-Control-Allow-Origin: HTTP
     * header), the image will be tainted and its usage restricted..
     */
    anonymous("anonymous"),
    /**
     * A cross-origin request (i.e. with Origin: HTTP header) is performed with
     * credential is sent (i.e. a cookie, a certificate and HTTP Basic
     * authentication is performed). If the server does not give credentials to
     * the origin site (through Access-Control-Allow-Credentials: HTTP header),
     * the image will be tainted and its usage restricted.
     */
    useCredentials("use-credentials");

    private final String value;

    private Crossorigin(String value) {
        this.value = value;
    }

    @Override
    public String toString() {
        return value;
    }

}
