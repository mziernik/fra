package com.exceptions;

import com.exceptions.intf.IDetailedException;
import java.util.LinkedHashMap;
import java.util.Map;

/**
 * Klasa niskopoziomowych wyjątków serwera związana np z konfiguracją usługi
 */
public class CoreException extends RuntimeException implements IDetailedException {

    private final Map<String, String> details = new LinkedHashMap<>();

    public CoreException(String message) {
        super(message);
    }

    public CoreException(String message, String details) {
        super(message);
        this.details(details);
    }

    public CoreException(String message, Throwable cause) {
        super(message, cause);
    }

    public CoreException(Throwable cause) {
        super(cause);
    }

    public CoreException details(String name, String details) {
        this.details.put(name, details);
        return this;
    }

    public CoreException details(String details) {
        this.details.put("", details);
        return this;
    }

    @Override
    public void getDetails(Map<String, String> details) {
        details.putAll(this.details);
    }
}
