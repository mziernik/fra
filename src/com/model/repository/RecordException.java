package com.model.repository;

public class RecordException extends RuntimeException {

    public RecordException(Record record, String message) {
        super((record != null ? record.getId() + ": " : "") +message);
    }

    public RecordException(Record record, String message, Throwable cause) {
        super((record != null ? record.getId() + ": " : "") + message, cause);
    }

    public RecordException(Record record, Throwable cause) {
        super(record != null ? record.getId() : null, cause);
    }

    public RecordException column(Column<?> col) {
        return this;
    }
}
