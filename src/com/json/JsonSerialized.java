package com.json;

import com.google.gson.GsonBuilder;
import com.lang.LJson;
import java.io.*;

/**
 * @author Miłosz Ziernik
 * @date 21 października 2015
 * @encoding UTF-8
 */
public class JsonSerialized {

    protected void read(InputStream in) throws IOException {
        JObject.parse(in).deserialize(this, getGson());
    }

    protected void readJs(String json) {
        JCollection js = JSON.parseJavaScript(json);
        if (js == null || !js.isObject())
            throw new UnsupportedOperationException(LJson.INVALID_FILE_FORMAT.toString());
        js.deserialize(this, getGson());
    }

    protected void save(OutputStream out) throws IOException {
        JSON.serialize(this, getGson()).write(out);
    }

    protected void save(File out) throws IOException {
        JElement el = JSON.serialize(this, getGson());
        el.getOptions().quotaNames(false).singleLine(false);
        el.write(out, false);
    }

    protected GsonBuilder getGson() {
        return new GsonBuilder();
    }
}
