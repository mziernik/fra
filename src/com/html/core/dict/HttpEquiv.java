package com.html.core.dict;

/**
 * @author Miłosz Ziernik
 * @date 26 sierpnia 2015
 * @encoding UTF-8
 */
public enum HttpEquiv {

    /**
     * Specifies the character encoding for the document. Example:
     *
     * <meta http-equiv="content-type" content="text/html; charset=UTF-8">
     */
    contentType("content-type"),
    /**
     * The value of the content attribute above must match the value of the
     * title attribute on a link element in the same document, or it must match
     * the value of the title attribute on a style element in the same document.
     */
    defaultStyle("default-style"),
    /**
     * Defines a time interval for the document to refresh itself. Example:
     *
     * <meta http-equiv="refresh" content="300">
     * Note: The value "refresh" should be used carefully, as it takes the
     * control of a page away from the user. Using "refresh" will cause a
     * failure in W3C's Web Content Accessibility Guidelines.
     *
     */
    refresh("refresh");

    private final String value;

    private HttpEquiv(String value) {
        this.value = value;
    }

    @Override
    public String toString() {
        return value;
    }

}
