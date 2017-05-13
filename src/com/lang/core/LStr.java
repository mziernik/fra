package com.lang.core;

import com.exceptions.ServiceException;
import java.util.HashMap;
import java.util.Map;

public class LStr implements LString {

    public final CharSequence value;
    public final Object[] arguments;

    final static Map<String, LanguageItem> items = new HashMap<>();

    public static LanguageItem define(String key) {
        LanguageItem item = new LanguageItem("Custom", key);

        if (Languages.allItems.containsKey(item.key))
            throw new ServiceException(String.format("Item %s alteady exists", item.key));

        Languages.allItems.put(item.id, item);
        items.put(key, item);
        return item;
    }

    public static LanguageItem getOrDefine(String key) {
        if (items.containsKey(key))
            return items.get(key);

        return define(key);
    }

    @Override
    public String toString() {
        return toString(arguments);
    }

    public LStr(CharSequence value, Object... arguments) {
        this.value = value;
        this.arguments = arguments;
    }

    @Override
    public String getValue(Language lang) {
        if (value instanceof LString)
            return ((LString) value).getValue(lang);
        return LString.super.getValue(lang);
    }

}
