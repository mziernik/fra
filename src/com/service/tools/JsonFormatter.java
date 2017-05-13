package com.service.tools;

import com.json.JElement;
import com.json.JSON;
import com.mlogger.Log;
import com.servlet.controller.Controller;
import com.servlet.interfaces.Endpoint;
import com.servlet.requests.HttpRequest;

@Endpoint(url = "$/tools/json")
public class JsonFormatter implements Controller {

    @Override
    public void onRequest(HttpRequest http) throws Exception {
        try {
            JElement json = JSON.parse(http.getInputStream());
            if (json.isCollection())
                json.asCollection().options.quotaNames(false);
            http.returnCustom(json.toString(), "application/javascript");
        } catch (Throwable e) {
            Log.warning(e);
            http.returnPlainText(e.getMessage(), 400);
        }
    }

}
