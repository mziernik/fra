package com.exceptions.intf;

import com.utils.Utils;
import com.utils.Is;
import com.exceptions.intf.IDetailedException;
import java.util.LinkedHashMap;
import java.util.Map;

public abstract class DetailedError
        extends Error
        implements IDetailedException {

    private final Map<String, String> details = new LinkedHashMap<>();

    public DetailedError() {
    }

    public DetailedError(String message) {
        super(message);
    }

    public DetailedError(String message, Throwable cause) {
        super(message, cause);
    }

    public DetailedError(Throwable cause) {
        super(cause);
    }

    public DetailedError details(String name, Object details) {
        this.details.put(name, Utils.toString(details));
        return this;
    }

    public DetailedError details(Object details) {
        this.details.put("", Utils.toString(details));
        return this;
    }

    @Override
    public void getDetails(Map<String, String> details) {
        details.putAll(this.details);
    }

}
