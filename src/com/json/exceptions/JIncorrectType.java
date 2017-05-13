package com.json.exceptions;

import com.utils.reflections.Reflections;
import com.exceptions.intf.IDetailedException;
import com.json.*;
import com.lang.LJson;
import com.utils.reflections.TClass;
import java.util.*;

public class JIncorrectType extends JException implements IDetailedException {

    private final String details;

    private static String getName(JElement el) {
        if (el == null)
            return null;
        if (el.isArray())
            return LJson.ARRAY.toString();
        if (el.isObject())
            return LJson.OBJECT.toString();
        if (el.isValue())
            return LJson.VALUE.toString();
        if (el.isNull())
            return LJson.NULL.toString();
        return null;
    }

    private static String getName(Class<? extends JElement> el) {
        if (el == null)
            return null;

        TClass<? extends JElement> clazz = new TClass<>(el);
        if (clazz.instanceOf(JArray.class))
            return LJson.ARRAY.toString();
        if (clazz.instanceOf(JObject.class))
            return LJson.OBJECT.toString();
        if (clazz.instanceOf(JValue.class))
            return LJson.VALUE.toString();
        return null;
    }

    public JIncorrectType(String name, JElement source, Class<? extends JElement> destination) {
        super(LJson.INVALID_ELEMENT_TYPE.toString() + (name != null ? " \"" + name + "\""
                : ""));
        details = LJson.PASSED_EXPECTED_ARGUMENT.toString(getName(source), getName(destination));

    }

    @Override
    public void getDetails(Map<String, String> details) {
        details.put("", this.details);
    }
}
