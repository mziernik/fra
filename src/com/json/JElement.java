package com.json;

import com.utils.Utils;
import com.utils.Is;
import com.exceptions.EError;
import com.exceptions.ThrowableException;
import com.lang.LJson;
import com.utils.collections.Strings;
import java.io.*;
import java.nio.charset.Charset;
import java.util.*;
import com.mlogger.Log;
import com.utils.reflections.TClass;
import com.utils.text.StrWriter;
import com.utils.text.WritableContent;

/**
 * Miłosz Ziernik 2014/04/15
 */
public abstract class JElement implements WritableContent, Cloneable {

    String name;
    String comment;
    boolean uncommented;
    public final Map<String, Object> extra = new LinkedHashMap<>();
    JCollection parent;
    String commentAfter;
    String commentBefore;
    String commentBeforePrefix;
    String commentBeforeSufix;
    String commentAfterSufix;
    String commentAfterPrefix;

    public abstract boolean remove();

    public abstract void move(JCollection destination);

    public abstract void moveChildren(JCollection destination);

    public JCollection getParent() {
        return parent;
    }

    @Override
    public JElement clone() {
        try {
            return (JElement) super.clone();
        } catch (CloneNotSupportedException ex) {
            throw new ThrowableException(ex);
        }
    }

    public Strings getPath() {
        Strings path = new Strings().separator("/");
        JElement item = this;
        JCollection col = parent;
        while (col != null) {
            boolean found = false;
            for (JElement el : col)
                if (el == item) {
                    if (col.isObject())
                        path.insert(new Escape().toString(item.getName()));
                    else if (col.isArray())
                        path.insert("[" + col.asArray().indexOf(item) + "]");
                    found = true;
                    break;
                }
            if (!found)
                break;
            item = col;
            col = col.parent;
        }
        return path;
    }

    public String getName() {
        return name;
    }

    public int getIndex() {
        return -1;
    }

    public boolean isCollection() {
        return this instanceof JCollection;
    }

    public JCollection asCollection() {
        return cast(JCollection.class);
    }

    public JObject asObject() {
        return cast(JObject.class);
    }

    public boolean isObject() {
        return this instanceof JObject;
    }

    public boolean isArray() {
        return this instanceof JArray;
    }

    public JArray asArray() {
        return cast(JArray.class);
    }

    public boolean isValue() {
        return this instanceof JValue;
    }

    public boolean isNull() {
        return this instanceof JNull;
    }

    public JValue asValue() {
        return cast(JValue.class);
    }

    private <T extends JElement> T cast(Class<T> cls) {
        if (!new TClass(getClass()).instanceOf(cls))
            throw EError.addDetails(
                    new RuntimeException(LJson.INVALID_ELEMENT_TYPE.toString() + "\n"
                            + LJson.PASSED_ARGUMENT.toString(getClass().getSimpleName())
                            + ", "
                            + LJson.EXPECTED_ARGUMENT.toString(cls.getSimpleName())),
                    LJson.VALUE.toString(), toString());

        return (T) this;
    }

    @Override
    public String toString() {
        StrWriter writer = new StrWriter();
        getContent(writer);
        return writer.toString();
    }

    public void write(Writer writer) throws IOException {
        StrWriter strw = new StrWriter(writer);
        getContent(strw);
        strw.flush();
    }

    public void write(OutputStream out) throws IOException {
        OutputStreamWriter writer = new OutputStreamWriter(out, Charset.forName("UTF-8"));
        write(writer);
    }

    public void write(File file, boolean bom) throws IOException {
        try (FileOutputStream out = new FileOutputStream(file)) {
            out.write(Utils.UTF8_BOM);
            JElement.this.write(out);
        }
    }

    void onException(Exception e) {
        Log.warning(e);
    }

    /*
     public void print(File file) throws IOException {
     try (BufferedWriter wr = getWriter(file);) {
     print(wr, "", 0, 1, true);
     }
     }
     */
    public JOptions getOptions() {
        return this instanceof JCollection ? ((JCollection) this).options
                : parent != null ? parent.options : new JOptions(null);
    }

    public int getLevel() {
        int level = 0;
        JElement el = parent;
        while (el != null) {
            ++level;
            el = el.parent;
        }
        return level;
    }

    protected void escape(String string, StrWriter w, JOptions opts) {
        try {
            new Escape()
                    .escapeUnicode(opts.escapeUnicode())
                    .singleQuota(opts.singleQuote())
                    .useQuota(false)
                    .toString(string, w);
        } catch (IOException e) {
            throw new ThrowableException(e);
        }
    }

    public JElement comment(String commnet) {
        this.comment = commnet;
        return this;
    }

    public JElement uncommented(boolean uncommented) {
        this.uncommented = uncommented;
        return this;
    }

    public void commentBefore(String comment) {
        this.commentBefore = comment;
    }

    public void commentBefore(String prefix, String comment, String sufix) {
        this.commentBefore = comment;
        this.commentBeforePrefix = prefix;
        this.commentBeforeSufix = sufix;
    }

    public void commentAfter(String after) {
        this.commentAfter = after;
    }

    public void commentAfter(String prefix, String comment, String sufix) {
        this.commentAfter = comment;
        this.commentAfterPrefix = prefix;
        this.commentAfterSufix = sufix;
    }

    public abstract boolean isEmpty();

    public String asString() {
        return isNull() ? null : asValue().asString();
    }

    public Number asNumber() {
        return isNull() ? null : asValue().asNumber();
    }

    public Boolean asBoolean() {
        return isNull() ? null : asValue().asBoolean();
    }

}
