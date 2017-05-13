package com.google.gson.adapter;

import com.google.gson.TypeAdapter;
import com.google.gson.stream.JsonReader;
import com.google.gson.stream.JsonWriter;
import com.utils.date.time.Interval;
import com.utils.date.time.Unit;
import java.io.IOException;

public class TTimeAdapter extends TypeAdapter<Interval> {

    @Override
    public void write(JsonWriter out, Interval value) throws IOException {
        if (value == null)
            out.nullValue();
        else {
            Unit precision = value.getPrecision();
            out.beginArray()
                    .value(value.getTime(precision))
                    .value(precision.name())
                    .endArray();
        }
    }

    @Override
    public Interval read(JsonReader in) throws IOException {
        throw new UnsupportedOperationException();
    }
}
