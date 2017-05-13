package com.test;

import com.servlet.controller.Page;
import com.exceptions.http.Http400BadRequestException;
import com.json.JArray;
import com.json.JObject;
import static com.resources.Res.*;
import com.servlet.interfaces.*;
import com.servlet.requests.HttpRequest;
import java.util.*;
import java.util.Map.Entry;

@ITestClass
@Endpoint(url = "$testjs", title = "Test modułów JavaScript")
public class JavaScript extends Page {

    @Override
    public void onRequest(HttpRequest http) throws Exception {
        link(utils, layer);
        link("/res/service/js_test.js");
    }

    @Endpoint()
    public void callback() throws Exception {
        Thread.sleep(params.getInt("sleep", 0));

        if (!params.getStr("exception", "").isEmpty())
            throw new Http400BadRequestException(params.getStr("exception", ""));

        JObject json = new JObject();
        Map<String, Set<Object>> map = params.getMap();
        for (Entry<String, Set<Object>> ee : map.entrySet())
            json.arrayC(ee.getKey()).addAll(ee.getValue());
        // returnJson(json);
    }

    @Endpoint(methods = HttpMethod.POST)
    public void test_ajax_post_params() throws Exception {

        List<String> lst = params.getList("arr");

        if (lst.size() != 3)
            throw new Exception("Nieprawidłowy rozmiar tablicy");

        JObject json = new JObject();

        json.put("str", params.getStr("string"));
        json.put("bool", params.getBool("bool"));
        json.put("int", params.getInt("int"));

        JArray jarr = json.arrayC("arr");
        for (String s : lst)
            jarr.add(s);

        //  returnJson(json);
    }

}
