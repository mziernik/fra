package com.servlet.requests;

import com.utils.StrUtils;
import com.servlet.UserAgent;

public class ContentDisposition {

    public boolean convertPolishChars = false;
    public Boolean inline = null; // tryb podglądu
    public EscapeMode escapeMode = null;
    private final HttpRequest request;

    public static enum EscapeMode {

        quota,
        uri,
        utf8Prefix
    }

    public ContentDisposition(HttpRequest request) {
        this.request = request;
        UserAgent.Browser group = request.userAgent.browser.getGroup();
        escapeMode = group == UserAgent.Browser.IE ? EscapeMode.uri
                : EscapeMode.utf8Prefix;
        convertPolishChars = escapeMode != EscapeMode.uri && escapeMode != EscapeMode.utf8Prefix;
    }

    public void setHeader(String fileName) {

        if (fileName == null || fileName.trim().isEmpty())
            return;

        if (convertPolishChars)
            fileName = StrUtils.convertPolishChars(fileName);

        boolean needEscape = false;
        for (char c : fileName.toCharArray())
            needEscape = c <= 32 || c > 127;

        if (needEscape && escapeMode != null)
            switch (escapeMode) {
                case quota:
                    fileName = "\"" + fileName + "\"";
                    break;
                case uri:
                case utf8Prefix:
                    fileName = StrUtils.encodeURIComponent(fileName);
                    break;
            }

        request.setHeader("Content-Disposition",
                (inline == null || Boolean.TRUE.equals(inline)
                ? "inline" : "attachment") + "; filename"
                + (needEscape && escapeMode != null && escapeMode == EscapeMode.utf8Prefix
                        ? "*=utf-8''" : "=")
                + StrUtils.formatFileName(fileName));

    }
}
