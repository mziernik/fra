package com.webapi;

import com.json.JObject;
import com.script.ConfFile;
import com.webapi.core.*;
import java.io.*;

public class WTools implements WebApi {

    public WTools() {

    }

    @WebApiEndpoint(dataType = DataType.STRING, description = "Evaluator")
    public String eval(WebApiRequest req) throws IOException {
        String src = req.getJson().asString();
        JObject json = new ConfFile(src).process();
        json.options.quotaNames(false);
        return json.toString();
    }

}
