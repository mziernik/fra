package com.config.engine.interfaces;

import com.config.engine.ConfigField;

@FunctionalInterface
public interface AfterChangeListener<Self extends ConfigField, RAW> {

    public void onChange(Self item, boolean isUserValue, RAW newValue) throws Exception;
}
