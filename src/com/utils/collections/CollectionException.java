package com.utils.collections;

public class CollectionException extends RuntimeException {

    public final CollectionExceptionType type;
    public final TCollection collection;

    public CollectionException(TCollection collection, CollectionExceptionType type, String message) {
        super(message);
        this.type = type;
        this.collection = collection;
    }

    public static enum CollectionExceptionType {
        INDEX_OUT_OF_BOUNDS,
        READ_ONLY,
        OVERLOAD,
        NULL
    }

}
