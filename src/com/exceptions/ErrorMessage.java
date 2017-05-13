package com.exceptions;

import java.util.*;

/**
 * @author Miłosz Ziernik
 * @date 24 września 2015
 * @encoding UTF-8
 */
public class ErrorMessage {

    public final String title;
    public final String message;
    public final boolean critical;
    public final List<String> errorStack = new LinkedList<>();
    public final Map<String, String> details = new LinkedHashMap<>();
    public final EError exception;
    public final boolean simple; // dane w formie uproszczonej (np na produkcji)

    public ErrorMessage(EError exception, boolean simple, boolean critial, String title, String message) {
        this.title = title;
        this.simple = simple;
        this.message = message;
        this.exception = exception;
        this.critical = critial;
    }

    @Override
    public String toString() {
        return message;
    }

}
