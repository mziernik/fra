package com.html.core.tag;

import com.utils.reflections.Reflections;
import com.utils.Utils;
import com.utils.Is;
import com.html.core.Html;
import com.html.core.HtmlAttributes;
import com.html.core.tag.intfs.*;
import com.html.js.core.JsAction;
import com.utils.collections.Strings;
import com.utils.hashes.Hashes;
import com.utils.reflections.TClass;
import java.util.LinkedList;
import java.util.List;

/**
 * @author Miłosz Ziernik
 * @date 24 września 2015
 * @encoding UTF-8
 */
public interface Tag<TTag extends Element> {

    public TTag getElement();

    default Tag<? extends Parent> getParent() {
        return getElement().parent;
    }

    default Element getParentElement() {
        Tag<? extends Parent> parent = getParent();
        return parent != null ? parent.getElement() : null;
    }

    default Visual<TTag> asVisual() {
        if (!(this instanceof Visual))
            throw new RuntimeException("Tag " + getPath(true) + " nie jest elementem wizualnym");
        return (Visual<TTag>) this;
    }

    default Container<TTag> asContainer() {
        if (!(this instanceof Container))
            throw new RuntimeException("Tag " + getPath(true) + " nie jest kontenerem");
        return (Container<TTag>) this;
    }

    default Tag<? extends Parent> getParent(int level) {
        Tag par = getParent();
        for (int i = 0; i < level; i++) {
            par = par.getParent();
            if (par == null)
                return null;
        }
        return par;
    }

    default Html getHTML() {
        Tag tag = this;
        while (tag != null) {
            if (tag.getElement() instanceof Html)
                return (Html) tag.getElement();
            tag = tag.getParent();
        }
        return null;
    }

    default String getName() {
        return getElement().name;
    }

    default Strings getPath(boolean inclIds) {
        Strings strs = new Strings().separator("/");

        Tag<?> tag = this;
        while (tag != null) {

            List<Tag> tags = new LinkedList<>();

            if (tag.getParent() != null && tag.getParent().getParent() != null)
                for (Tag t : tag.getParent().getChildren())
                    if (t.getName().equals(tag.getName()))
                        tags.add(t);

            int idx = tags.indexOf(tag) + 1;

            String id = inclIds ? tag.getId(false) : null;

            strs.insert(tag.getName() + (idx > 0 ? "[" + idx + "]" : "")
                    + (id != null && !id.isEmpty() ? "#" + id : ""));
            tag = tag.getParent();
        }

        return strs;
    }

    default String getId(boolean createIfNotExists) {

        HtmlAttributes.HtmlAttr<?> attr = getElement().attrs.get("id");

        String id = attr != null && attr instanceof HtmlAttributes.StrAttr
                ? ((HtmlAttributes.StrAttr) attr).value : null;

        if (createIfNotExists && (id == null || id.trim().isEmpty())) {
            id = Hashes.idHash6(getPath(false).toString("/"));
            getElement().attrs.set("id", id);
        }

        return id;
    }

    default HtmlAttributes<TTag> attrs() {
        return getElement().attrs;
    }

    default boolean isEmpty() {
        return Tag.this.getChildren().isEmpty()
                && attrs().isEmpty()
                && (getElement().innerHtml == null || getElement().innerHtml.isEmpty());
    }

    default LinkedList<Tag<? extends Element>> getChildren() {
        return getElement().children;
    }

    default <T extends Tag> LinkedList<T> getChildren(Class<T> cls) {
        LinkedList<T> list = new LinkedList<>();
        for (Tag el : Tag.this.getChildren())
            if (cls == null || new TClass(el.getClass()).instanceOf(cls))
                list.add((T) el);
        return list;
    }

    default TTag innerHTML(String html) {
        getElement().innerHtml = html;
        return (TTag) this;
    }

    default TTag attr(String name, String value) {
        return attrs().set(name, value).tag;
    }

    /**
     * Used to store custom data private to the page or application
     *
     * @param name
     * @param value
     * @return
     */
    default TTag data(String name, Object value) {
        return attrs().set("data-" + name, Utils.toString(value)).tag;
    }

    default TTag aria(String name, Object value) {
        return attrs().set("aria-" + name, Utils.toString(value)).tag;
    }

    default TTag comment(String comment) {
        this.getElement().comment = comment;
        return (TTag) this;
    }

    default TTag id(String id) {
        return attrs().set("id", id != null ? id.trim() : null).tag;
    }

    // ------------------------------ Zdarzenia ---------------
    default TTag onChange(JsAction... onChange) {
        return attrs().setAct("onchange", onChange).tag;
    }

    default boolean remove() {
        if (getParent() == null)
            return false;
        return getParent().getChildren().remove(this);
    }

    default TTag moveAfter(Tag tag) {
        if (tag == null || tag.getParent() == null)
            return (TTag) this;
        List<Tag<?>> children = tag.getParent().getChildren();
        if (getParent() != null)
            getParent().getChildren().remove(this);
        getElement().parent = tag.getParent().getElement();
        int idx = children.indexOf(tag);
        children.add(idx + 1, this);
        return (TTag) this;
    }

    default TTag moveBefore(Tag tag) {
        if (tag == null || tag.getParent() == null)
            return (TTag) this;
        List<Tag<?>> children = tag.getParent().getChildren();
        if (getParent() != null)
            getParent().getChildren().remove(this);
        getElement().parent = tag.getParent().getElement();
        int idx = children.indexOf(tag);
        children.add(idx, this);
        return (TTag) this;
    }

    default TTag moveTo(Parent tag) {
        if (tag == null)
            return (TTag) this;

        remove();
        this.getElement().parent = tag.getElement();
        tag.getChildren().add(this);
        return (TTag) this;
    }
}
