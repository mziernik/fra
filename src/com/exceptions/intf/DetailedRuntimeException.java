package com.exceptions.intf;

import java.util.LinkedHashMap;
import java.util.Map;

public abstract class DetailedRuntimeException
        extends RuntimeException
        implements IDetailedException {

    private final Map<String, String> details = new LinkedHashMap<>();

    public DetailedRuntimeException() {
    }

    public DetailedRuntimeException(String message) {
        super(message);
    }

    public DetailedRuntimeException(String message, Throwable cause) {
        super(message, cause);
    }

    public DetailedRuntimeException(Throwable cause) {
        super(cause);
    }

    public DetailedRuntimeException details(String name, String details) {
        this.details.put(name, details);
        return this;
    }

    public DetailedRuntimeException details(String details) {
        this.details.put("", details);
        return this;
    }

    @Override
    public void getDetails(Map<String, String> details) {
        details.putAll(this.details);
    }

}
