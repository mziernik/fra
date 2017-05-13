package com.exceptions.http;

import com.utils.Utils;
import com.utils.Is;
import com.servlet.requests.HttpRequest;

/**
 * Miłosz Ziernik 2013/11/19
 */
public class Http405MethodNotAllowed extends HttpException {

    public Http405MethodNotAllowed(String message) {
        super(405, message != null ? message : "Metoda nie dozwolona");
    }

    public Http405MethodNotAllowed(String message, String details) {
        super(405, message != null ? message : "Metoda nie dozwolona");
        this.details(details);
    }

    public Http405MethodNotAllowed(HttpRequest request) {
        super(405, "Metoda " + request.getMethod() + " nie jest dozwolona");
        this.details(Utils.toString(request.url));
    }
}
