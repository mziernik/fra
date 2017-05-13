package com.exceptions.intf;

import com.exceptions.intf.IDetailedException;
import java.util.LinkedHashMap;
import java.util.Map;

public abstract class DetailedException
        extends Exception
        implements IDetailedException {

    private final Map<String, String> details = new LinkedHashMap<>();

    public DetailedException() {
    }

    public DetailedException(String message) {
        super(message);
    }

    public DetailedException(String message, Throwable cause) {
        super(message, cause);
    }

    public DetailedException(Throwable cause) {
        super(cause);
    }

    public DetailedException details(String name, String details) {
        this.details.put(name, details);
        return this;
    }

    public DetailedException details(String details) {
        this.details.put("", details);
        return this;
    }

    @Override
    public void getDetails(Map<String, String> details) {
        details.putAll(this.details);
    }

}
