package com.google.gson.adapter;

import com.google.gson.TypeAdapter;
import com.google.gson.stream.JsonReader;
import com.google.gson.stream.JsonWriter;
import com.lang.core.LString;
import java.io.IOException;

public class LStringAdapter extends TypeAdapter<LString> {

    @Override
    public void write(JsonWriter out, LString value) throws IOException {
        out.value(value.toString());
    }

    @Override
    public LString read(JsonReader in) throws IOException {
        throw new UnsupportedOperationException();
    }

}
