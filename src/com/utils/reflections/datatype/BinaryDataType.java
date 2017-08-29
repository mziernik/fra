package com.utils.reflections.datatype;

import com.cache.CachedData;
import com.json.JObject;
import com.utils.Utils;

public class BinaryDataType extends DataType<CachedData> {

    public BinaryDataType(String type) {
        super(true, JsonType.OBJECT, type, "Dane binarne", CachedData.class, (value, parent) -> {

            if (value instanceof String) {
                CachedData cd = CachedData.get((String) value);
                if (cd == null)
                    throw new Error("Nie znaleziono zasobu " + Utils.escape(value));
                return cd;
            }

            return null;
        }, (CachedData cd) -> {
            if (cd == null)
                return null;

            JObject json = new JObject();
            json.options.singleLine();
            json.put("id", cd.key);
            json.put("name", cd.name);
            json.put("size", cd.length());
            json.put("preview", true);
            return json;
        });
    }

    public static BinaryDataType file() {
        return new BinaryDataType("file");
    }

    public static BinaryDataType image() {
        return new BinaryDataType("image");
    }
}
