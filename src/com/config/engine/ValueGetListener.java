package com.config.engine;

public interface ValueGetListener<Self extends ConfigField, Raw, Row> {

    public Raw onGet(Self item, Raw value, ValueSource source);
}
