package com.html.bootstrap.form;

import com.utils.Utils;
import com.utils.Is;
import com.html.core.dict.InputType;
import com.json.JArray;
import com.json.JValue;
import com.servlet.requests.HttpRequest;
import com.servlet.requests.RequestParams;
import java.util.LinkedHashMap;
import java.util.Map;

/**
 * @author Miłosz Ziernik
 * @date 24 września 2015
 * @encoding UTF-8
 */
public class FormField {

    public final String name;
    public String value;
    public final String id;
    String statusMessage;
    public Boolean disabled;
    public Boolean required;
    FieldStatus status;
    public final InputType type;
    public final Map<String, String> properties = new LinkedHashMap<>(); // atrybuty data-*
    public final Map<String, Object> extra = new LinkedHashMap<>();

    FormField(JArray arr, HttpRequest req) {

        //String id, String name, String value, String props
        this.id = Utils.coalesce(arr.element(0).asValue().asString(), "");

        String inputType = Utils.coalesce(arr.element(1).asValue().asString(), "");

        InputType type = null;
        for (InputType it : InputType.values())
            if (it.name().equalsIgnoreCase(inputType))
                type = it;
        this.type = type;

        this.name = Utils.coalesce(arr.element(2).asValue().asString(), "");
        this.value = Utils.coalesce(arr.element(3).asValue().asString(), "");

        for (JValue val : arr.element(4).asObject().getValues())
            properties.put(val.getName(), val.asString());

        RequestParams params = req.params;
        params.add(name, value);
    }

    public FormField setError(String message) {
        status = message != null ? FieldStatus.error : null;
        statusMessage = message;
        return this;
    }

    public FormField setStatus(FieldStatus status, String message) {
        this.status = status;
        statusMessage = message;
        return this;
    }
}
