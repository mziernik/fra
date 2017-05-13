package com.servlet.handlers.temporary;

import com.servlet.controller.Page;
import com.servlet.handlers.*;
import com.servlet.requests.HttpRequest;
import com.utils.date.time.Interval;

/**
 * Miłosz Ziernik 2014/04/01
 */
public class TempPage extends TempRequest {

    public final Class<? extends Page> page;

    public TempPage(Class<? extends Page> page, Interval expire) {
        super(page.getClass().getName(), expire);
        this.page = page;
    }

    @Override
    protected Class<? extends Page> getHandler(HttpRequest request) {
        return page;
    }

}
