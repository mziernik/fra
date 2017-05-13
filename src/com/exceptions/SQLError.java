package com.exceptions;

import com.exceptions.intf.IDetailedException;
import com.lang.LExceptions;
import java.sql.SQLException;
import java.util.LinkedHashMap;
import java.util.Map;

public class SQLError extends SQLException implements IDetailedException {

    private final Map<String, String> details = new LinkedHashMap<>();
    public final Map<String, String> data = new LinkedHashMap<>();
    public final Map<String, String> attributes = new LinkedHashMap<>();

    public SQLError(Throwable throwable, String message) {
        super(message, throwable);
    }

    public SQLError(Throwable cause) {
        super(cause);
    }

    public SQLError(String reason) {
        super(reason);
    }

    public SQLError details(String name, String details) {
        this.details.put(name, details);
        return this;
    }

    public SQLError details(String details) {
        this.details.put("", details);
        return this;
    }

    @Override
    public void getDetails(Map<String, String> details) {
        details.putAll(this.details);
    }

    public SQLError query(String query) {
        return details(LExceptions.QUERY.toString(), query);
    }
}
