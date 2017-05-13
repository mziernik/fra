package com.config.engine;



public class SingleConfigField<SELF extends SingleConfigField<SELF, RAW>, RAW>
        extends ConfigField<SELF, RAW, RAW> {

    public SingleConfigField(String key, CharSequence name, RAW defaultValue, ConfigCell<?, ?>... cells) {
        super(key, name, defaultValue, cells);
    }

}
