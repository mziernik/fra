package com.config.engine;

public interface ValueValidatorListener<Self extends ConfigField, Raw, Row> {

    public void validate(Self item, int idx, Row value) throws Exception;
}
