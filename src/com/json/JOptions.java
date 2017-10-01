package com.json;

/**
 * Miłosz Ziernik 2014/06/10
 */
public class JOptions {

    private Boolean escapeUnicode;
    private String intent;
    private Boolean acceptNulls;
    private Boolean quotaNames;
    private String lineBreakChar;
    private Boolean singleLine;
    private Boolean singleQuote;
    public final JCollection element;
    private Boolean readOnly;
    public static String defaultIntent = "\t";
    private Boolean javascriptMode;

    public JOptions(JCollection element) {
        this.element = element;
    }

    public boolean escapeUnicode() {
        JCollection el = element;
        while (el != null) {
            if (el.options.escapeUnicode != null)
                return el.options.escapeUnicode;
            el = el.parent;
        }
        return false;
    }

    public JOptions escapeUnicode(Boolean escapeUnicode) {
        this.escapeUnicode = escapeUnicode;
        return this;
    }

    public boolean acceptNulls() {
        JCollection el = element;
        while (el != null) {
            if (el.options.acceptNulls != null)
                return el.options.acceptNulls;
            el = el.parent;
        }
        return true;
    }

    public JOptions acceptNulls(Boolean acceptNulls) {
        this.acceptNulls = acceptNulls;
        return this;
    }

    public JOptions readOnly(Boolean readOnly) {
        this.readOnly = readOnly;
        return this;
    }

    public boolean readOnly() {
        JCollection el = element;
        while (el != null) {
            if (el.options.readOnly != null)
                return el.options.readOnly;
            el = el.parent;
        }
        return false;
    }

    public boolean quotaNames() {
        JCollection el = element;
        while (el != null) {
            if (el.options.quotaNames != null)
                return el.options.quotaNames;
            el = el.parent;
        }
        return true;
    }

    public JOptions compactMode(boolean compact) {
        singleLine(compact ? true : null).intent(compact ? "" : "  ");
        return this;
    }

    public JOptions javascriptMode(boolean javascriptMode) {
        this.javascriptMode = javascriptMode;

        if (javascriptMode) {
            if (quotaNames == null)
                quotaNames = false;
            if (singleQuote == null)
                singleQuote = true;
            intent = "  ";
        }

        return this;
    }

    public boolean javascriptMode() {
        JCollection el = element;
        while (el != null) {
            if (el.options.javascriptMode != null)
                return el.options.javascriptMode;
            el = el.parent;
        }
        return false;
    }

    public JOptions quotaNames(Boolean useQuota) {
        this.quotaNames = useQuota;
        return this;
    }

    public String intent() {
        JCollection el = element;
        while (el != null) {
            if (el.options.intent != null)
                return el.options.intent;
            el = el.parent;
        }
        return defaultIntent;
    }

    public JOptions intent(String intent) {
        this.intent = intent;
        return this;
    }

    public String lineBreakChar() {
        JCollection el = element;
        while (el != null) {
            if (el.options.lineBreakChar != null)
                return el.options.lineBreakChar;
            el = el.parent;
        }
        return "\n";
    }

    public JOptions lineBreakChar(String lineBreakChar) {
        this.lineBreakChar = lineBreakChar;
        return this;
    }

    public JOptions singleLine(Boolean singleLine) {
        this.singleLine = singleLine;
        return this;
    }

    public Boolean singleLine() {
        JCollection el = element;
        while (el != null) {
            if (el.options.singleLine != null)
                return el.options.singleLine;
            el = el.parent;
        }
        return null;
    }

    public JOptions singleQuote(boolean singleQuote) {
        this.singleQuote = singleQuote;
        return this;
    }

    public boolean singleQuote() {
        JCollection el = element;
        while (el != null) {
            if (el.options.singleQuote != null)
                return el.options.singleQuote;
            el = el.parent;
        }
        return false;
    }

}
