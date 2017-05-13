package com.exceptions.http;

/**
 * Miłosz Ziernik 2013/11/19
 */
public class Http401UnauthorizedException extends HttpException {

    public Http401UnauthorizedException(String message) {
        super(401, message);
    }

    public Http401UnauthorizedException(String message, String details) {
        super(401, message);
        this.details(details);
    }

    public Http401UnauthorizedException() {
        super(401, "Brak autoryzacji");
    }
}
