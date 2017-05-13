package com.servlet.handlers.temporary;

import com.servlet.controller.Page;
import com.html.core.Html;
import com.servlet.handlers.*;
import com.servlet.requests.HttpRequest;
import com.utils.date.time.Interval;

/**
 * Miłosz Ziernik 2014/04/01
 */
public class TempHtml extends TempRequest {

    public final Html html;

    public TempHtml(Html html, String name, Interval expire) {
        super(name, expire);
        this.html = html;
    }

    @Override
    protected Class<? extends Page> getHandler(HttpRequest request) {
        if (html != null) {
            request.properties.put("$req_html", html);
            return TempRequestPage.class;
        }
        return null;
    }
}
