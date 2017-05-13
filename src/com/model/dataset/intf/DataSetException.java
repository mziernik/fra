package com.model.dataset.intf;

public class DataSetException extends RuntimeException {

    public DataSetException(String message) {
        super(message);
    }

    public DataSetException(String message, Throwable cause) {
        super(message, cause);
    }

    public DataSetException(Throwable cause) {
        super(cause);
    }

}
