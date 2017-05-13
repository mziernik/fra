package com.exceptions.http;

import com.servlet.requests.HttpRequest;

/**
 * Miłosz Ziernik 2013/11/19
 */
public class PageNotFound extends Http404FileNotFoundException {

    public PageNotFound(String message, String details) {
        super(message, details);
    }

    public PageNotFound(HttpRequest request) {
        super(request);
    }
}
