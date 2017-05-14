package com.webapi.core.client;

import com.servlet.requests.HttpRequest;
import com.utils.text.StrWriter;
import com.webapi.core.WebApi;
import com.webapi.core.WebApiController;

public abstract class WebApiClientBuilder {

    protected final WebApiController controller;
    protected final StrWriter writer;

    public WebApiClientBuilder(WebApiController controller, StrWriter writer) {
        this.controller = controller;
        this.writer = writer;
    }

    public abstract void build();

    public abstract void build(HttpRequest http, Class<? extends WebApi> cls, String parent);
}
