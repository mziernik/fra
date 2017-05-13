package com.script.evaluator;

import com.utils.Utils;
import com.utils.Is;

/**
 * This exception is thrown when an error occurs during the evaluation process.
 */
public class EvaluationException extends RuntimeException {

    Pos pos;

    private static final long serialVersionUID = -3010333364122748053L;

    /**
     * This constructor takes a custom message as input.
     *
     * @param message A custom message for the exception to display.
     */
    public EvaluationException(String message) {
        super(message);
    }

    public EvaluationException(Pos pos, String message, Object... args) {
        super(String.format(pos == null ? message : message + " " + pos, args));
        this.pos = pos;
    }

    public EvaluationException(Pos pos, Throwable exception) {
        super(Utils.toString(pos) + " " + exception.getMessage(), exception);
        this.pos = pos;
    }

    /**
     * This constructor takes an exception as input.
     *
     * @param exception An exception.
     */
    public EvaluationException(Exception exception) {
        super(exception);
    }

    /**
     * This constructor takes an exception as input.
     *
     * @param message A custom message for the exception to display.
     * @param exception An exception.
     */
    public EvaluationException(String message, Exception exception) {
        super(message, exception);
    }
}
