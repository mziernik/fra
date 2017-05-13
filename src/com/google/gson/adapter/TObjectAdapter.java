package com.google.gson.adapter;

import com.google.gson.JsonDeserializationContext;
import com.google.gson.JsonDeserializer;
import com.google.gson.JsonElement;
import com.google.gson.JsonParseException;
import com.google.gson.JsonSerializationContext;
import com.google.gson.JsonSerializer;
import com.google.gson.TypeAdapter;
import com.google.gson.stream.JsonReader;
import com.google.gson.stream.JsonWriter;
import com.utils.TObject;
import java.io.IOException;
import java.lang.reflect.Type;

public class TObjectAdapter extends TypeAdapter<TObject>
        implements JsonSerializer<TObject>, JsonDeserializer<TObject> {

    @Override
    public void write(JsonWriter out, TObject value) throws IOException {
        //FixMe: TObjectAdapter obsługa różnego rodzaju obiektów
        out.value(value.toString());
    }

    @Override
    public TObject read(JsonReader in) throws IOException {
        return new TObject(in.nextString()); // ????
    }

    @Override
    public TObject deserialize(JsonElement json, Type typeOfT,
            JsonDeserializationContext context) throws JsonParseException {

        return null;
    }

    @Override
    public JsonElement serialize(TObject src, Type typeOfSrc, JsonSerializationContext context) {
        throw new UnsupportedOperationException("Not supported yet."); //To change body of generated methods, choose Tools | Templates.
    }

}
