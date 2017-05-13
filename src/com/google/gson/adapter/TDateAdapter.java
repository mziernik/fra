package com.google.gson.adapter;

import com.google.gson.TypeAdapter;
import com.google.gson.stream.JsonReader;
import com.google.gson.stream.JsonWriter;
import com.utils.date.TDate;
import java.io.IOException;
import java.text.ParseException;

public class TDateAdapter extends TypeAdapter<TDate> {

    @Override
    public void write(JsonWriter out, TDate value) throws IOException {
        if (value == null)
            out.nullValue();
        else
            out.value(value.toString());
    }

    @Override
    public TDate read(JsonReader in) throws IOException {
        try {
            return new TDate(in.nextString());
        } catch (ParseException ex) {
            throw new IOException(ex);
        }
    }
}
