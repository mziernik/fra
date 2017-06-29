package com.service.status;

import com.utils.reflections.datatype.DataType;

public class StatusItem<T> extends AbstractStatus<StatusItem<T>, T> {

    StatusItem(DataType<T> data, StatusGroup parent, String key, CharSequence name, CharSequence description) {
        super(false, data, parent, key, name, description);
    }

    public StatusItem<T> value(T value) {
        record.set(RStatus.VALUE, value).update();
        return this;
    }

}
