package com.model.repository;

public class RepositoryException extends RuntimeException {

    public RepositoryException(Repository<?> repo, String message) {
        super(message);
    }

    public RepositoryException(Repository<?> repo, String message, Throwable cause) {
        super(message, cause);
    }

    public RepositoryException(Repository<?> repo, Throwable cause) {
        super(cause);
    }
}
