package com.html.core;

import com.utils.text.StrWriter;
import com.html.core.tag.Element;
import com.utils.Utils;
import com.utils.Is;
import com.html.core.HtmlAttributes.HtmlAttr;
import com.html.js.JsActions;
import com.html.js.core.JsAction;
import com.utils.Url;
import com.utils.text.WritableContent;
import java.util.*;

/**
 * @author Miłosz Ziernik
 * @date 26 sierpnia 2015
 * @encoding UTF-8
 */
public class HtmlAttributes<TTag extends Element> implements Iterable<HtmlAttr> {

    public final TTag tag;
    private final Map<String, HtmlAttr> attributes = new LinkedHashMap<>();

    public HtmlAttributes(TTag el) {
        this.tag = el;
    }

    /**
     * Dowolny atrybut
     *
     * @param name
     * @param value
     * @return
     */
    public HtmlAttributes<TTag> set(String name, String value) {
        if (name != null && value != null)
            new StrAttr(this, name, value);
        return this;
    }

    /**
     * Atrubut typu link
     *
     * @param name
     * @param value
     * @return
     */
    public HtmlAttributes<TTag> setHref(String name, Url value) {
        if (name != null && value != null)
            new HrefAttr(this, name, value);
        return this;
    }

    public Url setHrefB(String name, String url) {
        if (name != null && url != null) {
            Url urlb = new Url(url);
            new HrefAttr(this, name, urlb);
            return urlb;
        }
        return null;
    }

    public HtmlAttributes<TTag> setAct(String name, JsAction... values) {
        if (name == null || values == null || values.length == 0)
            return this;
        new ActAttr(this, name, values.length == 1 ? values[0] : new JsActions(values));
        for (JsAction act : values)
            act.setTag(tag);
        return this;
    }

    public HtmlAttributes<TTag> setWritableAttr(String name, WritableContent value) {
        if (name != null && value != null)
            new WritableContentAttr(this, name, value);
        return this;
    }

    /*
     public HtmlAttributes<TTag> addAct(String name, JsAction value) {

     if (name == null || value == null)
     return this;

     value.setTag(tag);

     HtmlAttr attr = attributes.get(name);

     Actions acts = new Actions();

     if (attr instanceof ActAttr) {
     JsAction act = ((ActAttr) attr).value;

     if (act instanceof Actions)
     acts = (Actions) act;
     else
     if (act != null)
     acts.action(act);

     }

     acts.action(value);
     new ActAttr(this, name, acts);
     return this;
     }
     */
    /**
     * Atrybut, który wskazuje na id innego elementu. Jeśli id elementu
     * docelowego nie jest zdefiniowane, zostanie automatycznie wygenerowane
     *
     * @param name
     * @param destination
     * @return
     */
    public HtmlAttributes<TTag> setId(String name, Element destination) {
        if (name != null && destination != null)
            new IdAttr(this, name, destination);
        return this;
    }

    public HtmlAttributes<TTag> setNumber(String name, Number value) {
        if (name != null && value != null)
            new NumberAttr(this, name, value);
        return this;
    }

    public HtmlAttributes<TTag> setEnum(String name, Enum value) {
        if (name != null && value != null)
            new EnumAttr(this, name, value);
        return this;
    }

    /**
     * Atrybut typu boolean, istnieje gdy równy jest true
     *
     * @param name
     * @param value
     * @return
     */
    public HtmlAttributes<TTag> setState(String name, Boolean value) {
        if (name != null)
            if (Boolean.TRUE.equals(value))
                new StateAttr(this, name, value);
            else
                attributes.remove(name);
        return this;
    }

    public HtmlAttributes<TTag> setBool(String name, Boolean value) {
        if (name != null)
            if (Boolean.TRUE.equals(value))
                new BoolAttr(this, name, value);
            else
                attributes.remove(name);
        return this;
    }

    /**
     * Atrybut przyjmujący wartości on / off
     *
     * @param name
     * @param value
     * @return
     */
    public HtmlAttributes<TTag> setOnOff(String name, Boolean value) {
        if (name != null && value != null)
            new OnOffAttr(this, name, value);
        return this;
    }

    public HtmlAttr<?> get(String name) {
        return attributes.get(name);
    }

    public Object getValue(String name) {
        HtmlAttr attr = attributes.get(name);
        return attr != null ? attr.value : null;
    }

    @Override
    public Iterator<HtmlAttr> iterator() {
        return attributes.values().iterator();
    }

    public boolean isEmpty() {
        return attributes.isEmpty();
    }

    public boolean has(String name) {
        return name != null && attributes.containsKey(name.toLowerCase());
    }

    public void clear() {
        attributes.clear();
    }

    public abstract static class HtmlAttr<T> implements WritableContent {

        public final String name;
        protected final HtmlAttributes<?> attribs;
        public T value;

        public HtmlAttr(HtmlAttributes<?> attribs, String name, T value) {
            this.name = name.trim().toLowerCase();
            this.value = value;
            this.attribs = attribs;
            if (attribs != null)
                attribs.attributes.put(name, this);
        }

        @Override
        public void getContent(StrWriter writer) {
            if (value != null)
                writer.append(toString());
        }

        @Override
        public String toString() {
            return value != null ? Utils.toString(value) : null;
        }

        public boolean hasValue() {
            return value != null;
        }

    }

    public static class HrefAttr extends HtmlAttr<Url> {

        public HrefAttr(HtmlAttributes<?> attribs, String name, Url value) {
            super(attribs, name, value);
        }

        @Override
        public String toString() {
            if (value == null)
                return null;
            String url = value.toString();
            if (url != null && url.startsWith("/")) {
                Html html = attribs.tag.getHTML();
                if (html != null && html.getController().http() != null)
                    url = html.getController().http().getRelativePath(url).toString();
            }
            return url;
        }

    }

    public static class IdAttr extends HtmlAttr<Element> {

        public IdAttr(HtmlAttributes<?> attribs, String name, Element dst) {
            super(attribs, name, dst);
            dst.getId(true);
        }

        @Override
        public void getContent(StrWriter writer) {
            if (value == null)
                return;
            writer.append(value.getId(true));
        }

    }

    public static class ActAttr extends HtmlAttr<JsAction> {

        public ActAttr(HtmlAttributes<?> attribs, String name, JsAction value) {
            super(attribs, name, value);
        }

        @Override
        public void getContent(StrWriter writer) {
            if (value != null)
                value.getContent(writer);
        }

    }

    public static class WritableContentAttr extends HtmlAttr<WritableContent> {

        public WritableContentAttr(HtmlAttributes<?> attribs, String name, WritableContent value) {
            super(attribs, name, value);
        }

        @Override
        public void getContent(StrWriter writer) {
            if (value != null)
                value.getContent(writer);
        }

    }

    public static class StrAttr extends HtmlAttr<String> {

        public StrAttr(HtmlAttributes attribs, String name, String value) {
            super(attribs, name, value);
        }

    }

    public static class EnumAttr extends HtmlAttr<Enum> {

        public EnumAttr(HtmlAttributes<?> attribs, String name, Enum value) {
            super(attribs, name, value);
        }

    }

    public static class NumberAttr extends HtmlAttr<Number> {

        public NumberAttr(HtmlAttributes<?> attribs, String name, Number value) {
            super(attribs, name, value);
        }

    }

    /**
     * Atrybut zminimalizowany
     */
    public static class StateAttr extends HtmlAttr<Boolean> {

        public StateAttr(HtmlAttributes<?> attribs, String name, Boolean value) {
            super(attribs, name, value);

        }

        @Override
        public String toString() {
            return name;
        }

        @Override
        public boolean hasValue() {
            return false;
        }

    }

    public static class BoolAttr extends HtmlAttr<Boolean> {

        public BoolAttr(HtmlAttributes<?> attribs, String name, Boolean value) {
            super(attribs, name, value);

        }

    }

    public static class OnOffAttr extends HtmlAttr<Boolean> {

        public OnOffAttr(HtmlAttributes<?> attribs, String name, Boolean value) {
            super(attribs, name, value);
        }

        @Override
        public String toString() {
            return Boolean.TRUE.equals(value) ? "on"
                    : Boolean.FALSE.equals(value) ? "off"
                    : null;
        }
    }
}
