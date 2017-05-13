package com.servlet.modules;

import com.html.modules.WindowLayer;
import com.servlet.controller.Page;
import com.servlet.requests.HttpRequest;

@Deprecated
public abstract class LayerPage extends Page {

    @Override
    public void onRequest(HttpRequest http) throws Exception {
        onRequest(new WindowLayer(this));
    }

    protected abstract void onRequest(WindowLayer layer) throws Exception;
}
