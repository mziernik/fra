package com.servlet.handlers.temporary;

import com.servlet.controller.Page;
import com.servlet.requests.HttpRequest;
import com.utils.date.time.Interval;

/**
 * Miłosz Ziernik 2014/04/01
 */
public class TempResource extends TempRequest {

    protected byte[] buff;

    public TempResource(Interval expire, String name, byte[] data) {
        super(name, expire);
        this.buff = data;
    }

    @Override
    protected Class<? extends Page> getHandler(HttpRequest request) throws Exception {
        request.returnFile(buff, name, null, eTag);
        return null;
    }

}
