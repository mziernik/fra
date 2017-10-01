package com.service.status;

import com.utils.reflections.datatype.DataType;

public class StatusGroup extends AbstractStatus<StatusGroup, String> {

    public final static StatusGroup SERVICE = new StatusGroup("service", "Usługa", "Usługa");

    public StatusGroup(String key, CharSequence name, CharSequence description) {
        super(true, null, null, key, name, description);
    }

    StatusGroup(StatusGroup parent, String key, CharSequence name, CharSequence description) {
        super(true, null, parent, key, name, description);
    }

    public StatusItem<String> itemStr(String key, CharSequence name) {
        return new StatusItem<String>(DataType.STRING, this, key, name, null);
    }

    public <T> StatusItem<T> item(DataType<T> data, String key, CharSequence name) {
        return new StatusItem<T>(data, this, key, name, null);
    }

    public <T> StatusItem<T> item(DataType<T> data, String key, CharSequence name, CharSequence description) {
        return new StatusItem<T>(data, this, key, name, description);
    }

    public StatusGroup group(String key, CharSequence name) {
        return new StatusGroup(this, key, name, null);
    }

    public StatusGroup group(String key, CharSequence name, CharSequence description) {
        return new StatusGroup(this, key, name, description);
    }

    public StatusGroup remove(String key) {
        // usuń potomka
        return this;
    }

}
