package com.exceptions.http;

import com.utils.Utils;
import com.utils.Is;
import com.servlet.controller.Page;
import com.servlet.requests.HttpRequest;

/**
 * Miłosz Ziernik 2013/11/19
 */
public class Http400BadRequestException extends HttpException {

    public Http400BadRequestException(String message, String details) {
        super(400, message != null ? message : "Nieprawidłowe żądanie");
        details(details);
    }

    public Http400BadRequestException(String message) {
        super(400, message != null ? message : "Nieprawidłowe żądanie");
    }

    public Http400BadRequestException(HttpRequest http) {
        super(400, "Nieprawidłowe żądanie");
        details("URL", Utils.toString(http.url));
    }

    public Http400BadRequestException(Page page) {
        this(page.request);
    }
}
