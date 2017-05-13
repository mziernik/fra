package com.config.engine;

import com.config.engine.interfaces.CfgItemChangeSource;

public interface ValueChangeListener<Self extends ConfigField, Raw, Row> {

    public boolean onChange(Self item, Raw oldValue, Raw newValue, CfgItemChangeSource source) throws Exception;
}
