package com.config.engine.interfaces;

import com.config.engine.ConfigField;
import com.utils.TObject;

@FunctionalInterface
public interface BeforeChangeListener<Self extends ConfigField, RAW> {

    public boolean onChange(Self item, boolean isUserValue, RAW oldValue, TObject<RAW> newValue) throws Exception;
}
