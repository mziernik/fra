package com.cache;

import com.json.JArray;
import com.servlet.interfaces.Arg;
import com.webapi.core.WebApi;
import com.webapi.core.WebApiEndpoint;

public class WCache implements WebApi {

    @WebApiEndpoint()
    public JArray list() {
        return null;
    }

    @WebApiEndpoint()
    public void remove(@Arg(name = "id") String id) {

    }
}
