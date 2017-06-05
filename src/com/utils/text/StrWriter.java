package com.utils.text;

import com.exceptions.ThrowableException;
import com.intf.callable.Callable1;
import com.utils.Utils;
import com.json.Escape;
import java.io.*;
import java.util.*;

//ToDo: Funkcja autoIntent - jesli ustawiona po breakLine() przed zapisaniem danych należy wstawiać wcięcie
public class StrWriter extends StringWriter {

    // dowolne właściwości
    public final Map<String, Object> properties = new HashMap<>();
    private final Writer[] mirrors;
    private int length;
    //------------
    private boolean hasBr; // ostatni znak to enter
    private String intent = "    ";
    private boolean autoIntent;
    private String lineBreak = "\n";
    private boolean _isCompact;

    private boolean singleQuote;
    private int level;
    private int lineBreakCount = 1;

    private boolean memoryCopy;

    public StrWriter(Writer... mirrors) {
        this.mirrors = mirrors == null ? new Writer[0] : mirrors;
        memoryCopy = this.mirrors.length == 0;
    }

    public int getLevel() {
        return level;
    }

    public StrWriter setLevel(int level) {
        if (level >= 0)
            this.level = level;
        return this;
    }

    public boolean isEmpty() {
        return length == 0;
    }

    public int length() {
        return length;
    }

    public StrWriter memoryCopy(boolean memoryCopy) {
        this.memoryCopy = memoryCopy;
        return this;
    }

    public StrWriter singleQuote(boolean singleQuote) {
        this.singleQuote = singleQuote;
        return this;
    }

    @Override
    public StrWriter append(CharSequence csq) {
        super.append(csq);
        return this;
    }

    public StrWriter append(Object obj) {
        append(Utils.toString(obj, true));
        return this;
    }

    public <T> StrWriter join(Iterable<T> collection, String separator, Callable1<String, T> mapper) {
        boolean first = true;
        for (T t : collection) {
            String val = mapper.run(t);
            if (val == null)
                continue;
            if (!first)
                append(separator);
            append(val);
            first = false;
        }

        return this;
    }

    public StrWriter appendFrmt(String format, Object... args) {
        append(String.format(format, args));
        return this;
    }

    public StrWriter escape(Object object) {
        try {
            new Escape().useQuota(null).singleQuota(singleQuote).toString(object, this);
        } catch (IOException ex) {
            throw new RuntimeException();
        }
        return this;
    }

    @Override
    public StrWriter append(char c) {
        super.append(c);
        return this;
    }

    @Override
    public StrWriter append(CharSequence csq, int start, int end) {
        super.append(csq, start, end);
        return this;
    }

    @Override
    public void flush() {
        super.flush();
        for (Writer writer : mirrors)
            try {
                writer.flush();
            } catch (IOException ex) {
                throw new ThrowableException(ex);
            }
    }

    @Override
    public void close() throws IOException {
        super.close();
        for (Writer writer : mirrors)
            try {
                writer.close();
            } catch (IOException ex) {
                throw new ThrowableException(ex);
            }
    }

    @Override
    public void write(String str) {
        if (autoIntent && hasBr && !str.equals(lineBreak)) {
            hasBr = false;
            intent();
        }

        if (memoryCopy)
            super.write(str);

        if (str != null)
            length += str.length();

        for (Writer writer : mirrors)
            try {
                writer.append(str);
            } catch (IOException ex) {
                throw new ThrowableException(ex);
            }
    }

    @Override
    public void write(char[] cbuf) throws IOException {
        
        if (memoryCopy)
            super.write(cbuf);

        if (cbuf != null)
            length += cbuf.length;

        for (Writer writer : mirrors)
            try {
                writer.append(new String(cbuf));
            } catch (IOException ex) {
                throw new ThrowableException(ex);
            }
    }

    @Override
    public void write(int c) {
        if (memoryCopy)
            super.write(c);

        length += 1;

        for (Writer writer : mirrors)
            try {
                writer.append((char) c);
            } catch (IOException ex) {
                throw new ThrowableException(ex);
            }
    }

    @Override
    public void write(String str, int off, int len) {
        if (memoryCopy)
            super.write(str, off, len);

        length += len;

        for (Writer writer : mirrors)
            try {
                writer.append(str, off, len);
            } catch (IOException ex) {
                throw new ThrowableException(ex);
            }
    }

    @Override
    public void write(char[] cbuf, int off, int len) {
        if (memoryCopy)
            super.write(cbuf, off, len);

        length += len;

        for (Writer writer : mirrors)
            try {
                writer.append(new String(cbuf), off, len);
            } catch (IOException ex) {
                throw new ThrowableException(ex);
            }
    }

    // =========================== budowanie treści =============================
    public boolean isCompact() {
        return _isCompact;
    }

    public StrWriter setIntent(String intent) {
        this.intent = intent;
        return this;
    }

    public StrWriter setAutoIntent(boolean autoIntent) {
        this.autoIntent = autoIntent;
        return this;
    }

    public StrWriter setLineBreak(String lineBreak) {
        this.lineBreak = lineBreak;
        _isCompact = !(lineBreak != null && (lineBreak.contains("\n") || lineBreak.contains("\r")));
        return this;
    }

    public StrWriter intent(int level) {
        if (!isCompact() && level > 0 && intent != null && !intent.isEmpty())
            for (int i = 0; i < level; i++)
                append(intent);
        return this;
    }

    public StrWriter intent() {
        return intent(level);
    }

    public StrWriter lineBreak(String compactModeLineBreak) {
        if (!isCompact() && lineBreak != null) {
            write(lineBreak);
            ++lineBreakCount;
        }
        if (isCompact() && compactModeLineBreak != null)
            write(compactModeLineBreak);
        return this;
    }

    public StrWriter text(Object value) {
        String text = Utils.toString(value);
        if (text == null || text.isEmpty())
            return this;

        String[] lines = text.split("\\n");
        for (int i = 0; i < lines.length; i++) {
            if (i > 0)
                br().intent();
            append(lines[i]);
        }
        return this;
    }

    public StrWriter nextLevel(Runnable runnable) {
        int oryginalLevel = level;
        ++level;
        runnable.run();
        level = oryginalLevel;
        return this;
    }

    public StrWriter br() {
        lineBreak(null);
        this.hasBr = true;
        return this;
    }

    /**
     * Zwraca znak łamania linii. W trybie kompaktowym zwracana jest spacja.
     *
     * @return
     */
    public String getLineBreak() {
        return lineBreak;
    }

    public String getIntent() {
        return intent;
    }

    public int getLineBreakCount() {
        return lineBreakCount;
    }

}
