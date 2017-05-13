package com.exceptions.http;

/**
 * Miłosz Ziernik 2013/11/19
 */
public class Http504GatewayTimeout extends HttpException {

    public Http504GatewayTimeout(String message, String details) {
        super(504, message != null ? message : "Przekroczony czas bramy");
        this.details(details);
    }

    public Http504GatewayTimeout() {
        super(504, "Przekroczony czas bramy");
    }
}
