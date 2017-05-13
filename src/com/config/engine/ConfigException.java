package com.config.engine;

public class ConfigException extends RuntimeException {

    public final ConfigField field;

    public ConfigException(String message) {
        super(message);
        this.field = null;
    }

    public ConfigException(ConfigField field, String message) {
        super("[" + field.getKey() + "]: " + message);
        this.field = field;
    }

    public ConfigException(ConfigField field, String message, Throwable cause) {
        super("[" + field.getKey() + "]: " + message, cause);
        this.field = field;
    }

    public ConfigException(String message, Throwable cause) {
        super(message, cause);
        this.field = null;
    }

    public ConfigException(Throwable cause) {
        super(cause);
        this.field = null;
    }

    public ConfigException(ConfigField field, Throwable cause) {
        super(cause);
        this.field = field;
    }

}
