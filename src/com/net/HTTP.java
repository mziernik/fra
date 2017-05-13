package com.net;

/**
 * @author Miłosz Ziernik
 * @date 17 września 2015
 * @encoding UTF-8
 */
public enum HTTP {

    ok200(200),
    /**
     * HTTP Status-Code 201: Created.
     */
    created201(201),
    /**
     * HTTP Status-Code 202: Accepted.
     */
    accepted202(202),
    /**
     * HTTP Status-Code 203: Non-Authoritative Information.
     */
    nonAuthoritative203(203),
    /**
     * HTTP Status-Code 204: No Content.
     */
    noContent204(204),
    /**
     * HTTP Status-Code 205: Reset Content.
     */
    reset205(205),
    /**
     * HTTP Status-Code 206: Partial Content.
     */
    partial206(206),
    /* 3XX: relocation/redirect */
    /**
     * HTTP Status-Code 300: Multiple Choices.
     */
    multiplechoices300(300),
    /**
     * HTTP Status-Code 301: Moved Permanently.
     */
    movedPermanently301(301),
    /**
     * HTTP Status-Code 302: Temporary Redirect.
     */
    temporaryRedirect302(302),
    /**
     * HTTP Status-Code 303: See Other.
     */
    SseeOther303(303),
    /**
     * HTTP Status-Code 304: Not Modified.
     */
    notModified304(304),
    /**
     * HTTP Status-Code 305: Use Proxy.
     */
    useProxy305(305),
    /* 4XX: client error */
    /**
     * HTTP Status-Code 400: Bad Request.
     */
    badRequest400(400),
    /**
     * HTTP Status-Code 401: Unauthorized.
     */
    unauthorized401(401),
    /**
     * HTTP Status-Code 402: Payment Required.
     */
    paymentRequired402(402),
    /**
     * HTTP Status-Code 403: Forbidden.
     */
    forbidden403(403),
    /**
     * HTTP Status-Code 404: Not Found.
     */
    notFound404(404),
    /**
     * HTTP Status-Code 405: Method Not Allowed.
     */
    notAllowed405(405),
    /**
     * HTTP Status-Code 406: Not Acceptable.
     */
    notAcceptable406(406),
    /**
     * HTTP Status-Code 407: Proxy Authentication Required.
     */
    proxyAuthentication407(407),
    /**
     * HTTP Status-Code 408: Request Time-Out.
     */
    requestTimeOut408(408),
    /**
     * HTTP Status-Code 409: Conflict.
     */
    conflict409(409),
    /**
     * HTTP Status-Code 410: Gone.
     */
    gone410(410),
    /**
     * HTTP Status-Code 411: Length Required.
     */
    lengthRequired411(411),
    /**
     * HTTP Status-Code 412: Precondition Failed.
     */
    preconditionFailed412(412),
    /**
     * HTTP Status-Code 413: Request Entity Too Large.
     */
    entityTooLarge413(413),
    /**
     * HTTP Status-Code 414: Request-URI Too Large.
     */
    requestURriTooLarge414(414),
    /**
     * HTTP Status-Code 415: Unsupported Media Type.
     */
    unsupportedMediaType415(415),
    /* 5XX: server error */
    /**
     * HTTP Status-Code 500: Internal Server Error.
     */
    internalServerError500(500),
    /**
     * HTTP Status-Code 501: Not Implemented.
     */
    notImplemented501(501),
    /**
     * HTTP Status-Code 502: Bad Gateway.
     */
    badGateway502(502),
    /**
     * HTTP Status-Code 503: Service Unavailable.
     */
    serviceUnavailable503(503),
    /**
     * HTTP Status-Code 504: Gateway Timeout.
     */
    gatewayTimeout504(504),
    /**
     * HTTP Status-Code 505: HTTP Version Not Supported.
     */
    httpVersionNotSupported505(505);

    public final int code;

    private HTTP(int code) {
        this.code = code;
    }

}
