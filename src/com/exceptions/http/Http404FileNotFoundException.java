package com.exceptions.http;

import com.utils.Utils;
import com.utils.Is;
import com.servlet.controller.Page;
import com.servlet.requests.HttpRequest;

/**
 * Miłosz Ziernik 2013/11/19
 */
public class Http404FileNotFoundException extends HttpException {

    public Http404FileNotFoundException(String message, String details) {
        super(404, message != null ? message : "Nie znaleziono pliku");
        this.details(details);
    }

    public Http404FileNotFoundException(String message) {
        super(404, message != null ? message : "Nie znaleziono pliku");
    }

    public Http404FileNotFoundException(HttpRequest request) {
        super(404, "Nie znaleziono pliku \"" + request.relativePath
                + (request.getQueryString() != null ? "?"
                + request.getQueryString() : "") + "\"");
        this.details(Utils.toString(request.url));
    }

    public Http404FileNotFoundException(Page page) {
        this(page.request);
    }
}
