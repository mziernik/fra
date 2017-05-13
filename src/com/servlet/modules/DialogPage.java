package com.servlet.modules;

import com.html.modules.LayerDialog;
import com.servlet.controller.Page;
import com.servlet.requests.HttpRequest;

@Deprecated
public abstract class DialogPage extends Page {

    @Override
    public void onRequest(HttpRequest http) throws Exception {
        onRequest(new LayerDialog(this));
    }

    protected abstract void onRequest(LayerDialog layer) throws Exception;

}
