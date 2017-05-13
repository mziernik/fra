package com.json;

import com.google.gson.*;
import com.google.gson.stream.JsonReader;
import com.utils.Utils;
import com.utils.text.StrWriter;
import java.io.StringReader;
import java.lang.reflect.Type;
import java.util.*;
import static com.utils.Utils.coalesce;
import com.utils.collections.TList;

/**
 * Miłosz Ziernik 2014/04/15
 */
public abstract class JCollection extends JElement
        implements Iterable<JElement> {

    public final JOptions options = new JOptions(this);

    abstract JElement addElement(String name, JElement el, boolean insert);

    public abstract void clear();

    public abstract int size();

    public abstract Collection<JElement> asRawCollection();

    public abstract TList<JElement> getElements();

    @Override
    public abstract boolean isEmpty();

    public abstract void sort();

    public abstract void invert();

    public abstract void sort(Comparator<JElement> comparator);

    abstract boolean doRemove(Object obj);

    public void deserialize(Object destination) {
        deserialize(destination, null);
    }

    public <T> void deserialize(T destination, GsonBuilder builder) {
        if (builder == null)
            builder = new GsonBuilder();

        Class<?> type = destination.getClass();

        JSON.registerAdapters(builder);

        JsonReader reader = new JsonReader(new StringReader(toString()));
        builder.registerTypeAdapter(type, new InstanceCreator<T>() {
            @Override
            public T createInstance(Type t) {
                return destination;
            }
        }).create().fromJson(reader, type);

    }

    public <T> T deserializeCls(Class<T> destination) {
        return deserializeCls(destination, null);
    }

    public <T> T deserializeCls(Class<T> destination, GsonBuilder builder) {
        if (builder == null)
            builder = new GsonBuilder();

        JsonReader reader = new JsonReader(new StringReader(toString()));
        return builder.create().fromJson(reader, destination);
    }

    public TList<JObject> getObjects() {
        TList<JObject> list = new TList<>();
        for (JElement el : this)
            if (el.isObject())
                list.add(el.asObject());
        return list;
    }

    public TList<JValue> getValues() {
        TList<JValue> list = new TList<>();
        for (JElement el : this)
            if (el.isValue())
                list.add(el.asValue());
        return list;
    }

    public TList<String> getValuesStr() {
        TList<String> list = new TList<>();
        for (JElement el : this)
            if (el.isValue())
                list.add(el.asValue().asString());
        return list;
    }

    public <T> TList<T> getValues(Class<T> cls) {
        TList<T> list = new TList<>();
        for (JElement el : this)
            if (el.isValue()
                    && el.asValue().value() != null
                    && cls.isAssignableFrom(el.asValue().value().getClass()))
                list.add((T) el.asValue().value());
        return list;
    }

    public TList<JArray> getArrays() {
        TList<JArray> list = new TList<>();
        for (JElement el : this)
            if (el.isArray())
                list.add(el.asArray());
        return list;
    }

    public JCollection arrayC(String name) {
        if (this instanceof JObject)
            return asObject().arrayC(name);

        if (this instanceof JArray)
            return asArray().array();
        return null;
    }

    public JCollection objectC(String name) {
        if (this instanceof JObject)
            return asObject().objectC(name);

        if (this instanceof JArray)
            return asArray().object();
        return null;
    }

    /*
     public JObject object(final String name) {
     if (this instanceof JObject)
     return asObject().object(names);

     if (this instanceof JArray)
     return asArray().object();
     return null;
     }

     public JArray getArray(final String name, Boolean canCreate) {
     if (this instanceof JObject)
     return asObject().array(name, canCreate);

     if (this instanceof JArray)
     return asArray().array();
     return null;
     }
     */
    public JCollection value(final String name, final Object value) {
        if (this instanceof JObject)
            asObject().put(name, value);

        if (this instanceof JArray)
            asArray().add(value);

        return this;
    }

    @Override
    public void getContent(StrWriter writer) {

        char begin = isArray() ? '[' : '{';
        char end = isArray() ? ']' : '}';

        JOptions opt = getOptions();
        String intent = opt.intent();
        boolean compact = intent.isEmpty() && (comment == null || comment.isEmpty());

        Boolean sl = opt.singleLine();

        boolean singleLine = (sl != null && sl == true)
                || (sl == null && isArray() && asArray().hasOnlyValues());

        boolean comm = comment != null && !comment.isEmpty();

        if ((sl == null || Boolean.TRUE.equals(sl)) && comm)
            singleLine = false;

        if (singleLine)
            comm = false;

        String lineBreak = compact || singleLine ? "" : opt.lineBreakChar();

        if (parent == null && commentBefore != null)
            writer.append(coalesce(commentBeforePrefix, ""))
                    .append("/*")
                    .append(commentBefore)
                    .append("*/")
                    .append(coalesce(commentBeforeSufix, ""))
                    .append(compact ? "" : singleLine ? " " : lineBreak);

        writer.append(begin);
        if (isEmpty()) {
            writer.append(end);
            return;
        }

        if (comm)
            writer.append(" //")
                    .append(comment.replace("\n", " ").replace("\r", ""));

        writer.append(lineBreak);

        char qChar = options.singleQuote() ? '\'' : '"';

        JElement[] arr = asRawCollection().toArray(new JElement[0]);
        for (int i = 0; i < arr.length; i++) {
            JElement el = arr[i];

            int lvl = writer.getLevel();
            writer.setLevel(lvl + 1);

            // JElement next = i < list.size() - 1 ? list.get(i + 1) : null;
            //   if (json.writeIntf != null && !json.writeIntf.beforeWriteName(writer, el, name))
            //       continue;
            if (el.commentBefore != null)
                writer.append(coalesce(el.commentBeforePrefix, ""))
                        .intent()
                        .append("/*")
                        .append(el.commentBefore)
                        .append("*/")
                        .append(coalesce(el.commentBeforeSufix, ""))
                        .append(compact ? "" : singleLine ? " " : lineBreak);

            if (!singleLine && !compact)
                writer.intent();

            if ((el instanceof JValue || el instanceof JNull) && el.uncommented)
                writer.append("//");

            if (el instanceof JCollection && el.uncommented)
                writer.append("/* ");

            if (isObject()) {
                boolean quot = useQuota(el.name);
                if (quot) {
                    writer.append(qChar);
                    escape(el.name, writer, opt);
                    writer.append(qChar);
                } else
                    writer.append(el.name);
                writer.append(":");
                if (!compact)
                    writer.append(" ");
            }

            el.getContent(writer);

            int j = i;
            JElement nextNonComment = Utils.next(arr, j);
            while (nextNonComment != null && nextNonComment.uncommented)
                nextNonComment = Utils.next(arr, ++j);

            if (nextNonComment != null)
                writer.append(",");

            if ((el instanceof JValue || el instanceof JNull)
                    && el.comment != null && !el.comment.isEmpty()) {
                writer.append(" //");
                escape(el.comment, writer, options);
            }

            if (el instanceof JCollection && el.uncommented)
                writer.append(" */");

            if (Utils.next(arr, i) != null)
                writer.append(compact ? "" : singleLine ? " " : lineBreak);

            if (el.commentAfter != null)
                writer.append(compact ? "" : singleLine ? " " : lineBreak)
                        .append(coalesce(el.commentAfterPrefix, ""))
                        .intent()
                        .append("/*")
                        .append(el.commentAfter)
                        .append("*/")
                        .append(coalesce(el.commentAfterSufix, ""));

            writer.setLevel(lvl);

        }

        if (!singleLine)
            writer.append(lineBreak)
                    .intent();

        writer.append(end);

        if (parent == null && commentAfter
                != null)
            writer.append(compact
                    ? "" : singleLine ? " " : lineBreak)
                    .append(coalesce(commentAfterPrefix, ""))
                    .append("/*")
                    .append(commentAfter)
                    .append("*/")
                    .append(coalesce(commentAfterSufix, ""));
    }

    /**
     * Złącz dwie kolekcje
     *
     * @param second Druga kolekcja (musi być tego samego typu: JObject lub
     * JArray
     * @param left jeśli nazwy się pokrywają, zachowaj lewy (oryginalny)
     * element, w przeciwnym razie nadpisz
     */
    public abstract void join(JCollection second, boolean left);

    boolean useQuota(String str) {
        if (options.quotaNames())
            return true;

        boolean jsMode = options.javascriptMode();

        if (str == null)
            return false;

        if (str.trim().isEmpty())
            return true;

        for (int i = 0; i < str.length(); i++) {
            boolean ok = true;
            char c = str.charAt(i);
            ok = (c >= 48 && c <= 52) || (c >= 65 && c <= 90)
                    || (c >= 97 && c <= 122) || (c == '_')
                    || (!jsMode && (c == '.' || c == '-'));

            if (!ok)
                return true;
            if (i == 0 && c >= 48 && c <= 52)
                return true;
        }
        return false;
    }

    /* String ln() {
     return compactMode != null && compactMode ? "" : "\n";
     } */
    public JCollection getRoot() {

        JCollection col = this;

        while (col.parent != null)
            col = col.parent;

        return col;
    }

}
