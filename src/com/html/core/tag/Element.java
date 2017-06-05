package com.html.core.tag;

import com.utils.Utils;
import com.utils.Is;
import com.utils.text.StrWriter;
import com.context.AppContext;
import com.html.core.Html;
import com.html.core.HtmlAttributes;
import com.html.core.HtmlAttributes.ActAttr;
import com.html.core.HtmlAttributes.HtmlAttr;
import com.html.core.styles.Selector;
import com.html.core.tag.form.Form;
import com.html.core.tag.intfs.*;
import com.html.core.tag.semantic.CTag;
import com.html.js.JsActions;
import com.html.js.core.JsAction;
import com.servlet.controller.Controller;
import com.utils.*;
import com.utils.text.WritableContent;
import java.util.*;

/**
 * Klasa określająca dowolny element HTML oraz implementująca ich metody
 * (atrybuty, innerText itd.).
 *
 * @param <TTag> Parametr określający typ zwracanego taga po wywołaniu metody
 * modyfikującej atrybut.
 */
public abstract class Element<TTag extends Element>
        implements WritableContent, Tag<TTag>, Cloneable {

    public final Map<String, Object> properties = new HashMap<>(); // dowolne wartosci
    //------------------------------
    @SuppressWarnings("unchecked")
    protected final TTag self = (TTag) this;
    private Selector style;
    String comment;
    protected Tag<? extends Parent> parent;
    String name;
    final LinkedList<Element<?>> children = new LinkedList<>();
    //private String innerText;
    protected String innerHtml;
    protected JsAction innerAction;
    public final Node<TTag> node = new Node<>(self);

    public final HtmlAttributes<TTag> attrs = new HtmlAttributes<>(self);

    protected void requiredAttribute(String... attribute) {

    }

    protected void requiredTag(String... attribute) {

    }

    // konstruktor dla Text
    protected Element(Tag<? extends Parent> parent, String name) {
        this.parent = parent;
        this.name = name.trim().toLowerCase();

        if (parent != null) {
            parent.getChildren().add(this);
            parent.getElement().node.onCreateTag(self);
        }
    }

    @Override
    public TTag getElement() {
        return self;
    }

    protected TTag alt(String alt) {
        return attrs.set("alt", alt).tag;
    }

    protected TTag data(Url data) {
        return attrs.setHref("data", data).tag;
    }

    protected TTag disabled(boolean disabled) {
        return attrs.setState("disabled", disabled).tag;
    }

    /**
     * Specifies one or more forms the <input> element belongs to
     *
     * @param form
     * @return
     */
    protected TTag form(Form form) {
        return attrs.setId("form", form).tag;
    }

    protected TTag href(Url href) {
        return attrs.setHref("href", href).tag;
    }

    protected Url hrefB(String href) {
        return attrs.setHrefB("href", href);
    }

    protected TTag href(Class<? extends Controller> page) {
        return attrs.setHref("href", new Url(page)).tag;
    }

    protected Url hrefB(Class<? extends Controller> page) {
        Url url = new Url(page);
        attrs.setHref("href", url);
        return url;
    }

    protected Url hrefB(Controller page) {
        Url url = new Url(page);
        attrs.setHref("href", url);
        return url;
    }

    protected TTag readOnly(boolean readonly) {

        return attrs.setState("readonly", readonly).tag;
    }

    protected TTag height(Number height) {
        return attrs.setNumber("height", height).tag;
    }

    protected TTag width(Number width) {
        return attrs.setNumber("width", width).tag;
    }

    protected TTag src(Url src) {
        return attrs.setHref("src", src).tag;
    }

    protected TTag name(String name) {
        return attrs.set("name", name).tag;
    }

    protected Url srcB(String src) {
        return attrs.setHrefB("src", src);
    }

    protected TTag target(String framename) {
        return attrs.set("target", framename).tag;
    }

    protected TTag value(Object value) {
        return attrs.set("value", Utils.toString(value)).tag;
    }

    void _visitTag(TagVisitor visitor) {
        if (!visitor.visitTag(this))
            return;

        for (Element t : children)
            t._visitTag(visitor);
    }

    @Override
    public TTag clone() {
        return self;
    }

    public TTag clear() {
        children.clear();
        attrs.clear();
        return self;
    }

    public class Helper {

        public void setInnerActions(JsAction... actions) {
            if (actions == null || actions.length == 0)
                return;
            Element.this.innerAction = actions.length == 1 ? actions[0]
                    : new JsActions(actions);
            for (JsAction act : actions)
                act.setTag(Element.this);
        }

        public Selector style() {
            if (style == null) {
                style = new Selector(Element.this);
                attrs.setWritableAttr("style", style);
            }
            return style;
        }

        public Iterator<Element<?>> iterator() {
            return children.iterator();
        }
    }

    // ========================== BUILDER ==================================
    public class BuildOptions {

        public boolean xhtml;
        public boolean compactJS;
        //public Boolean closed; // <br/>  zamiast <br></br>
        public String intent;
        public Boolean escapeReturn; // zamień enter na &#xa;

        public String intent() {
            return intent != null ? intent : "    ";
        }

    }

    // =========================================================================
    @Override
    public String toString() {
        StrWriter writer = new StrWriter();
        getContent(writer);
        return writer.toString();
    }

    @Override
    public void getContent(StrWriter writer) {

        for (BuildListener<TTag> event : node.beforeBuildEvents)
            if (!event.onBeforeBuildTag(self))
                return;

        if (comment != null)
            writer.append("<!-- ")
                    .append(comment)
                    .append(" -->")
                    .br().intent();

        Object prop = writer.properties.get("xhtml");
        boolean xhtml = prop instanceof Boolean ? (Boolean) prop : false;

        if (!(writer.properties.get("prev-tag") instanceof Text))
            writer.intent();

        writer.append("<").append(name);

        for (HtmlAttr<?> attr : attrs) {

            writer.append(" ").append(attr.name);
            if (xhtml || attr.hasValue()) {
                writer.append("=").append('"');
                StrWriter wr = new StrWriter();

                boolean compact = !(attr instanceof ActAttr);

                if (compact)
                    wr.setLineBreak("");

                wr.nextLevel(() -> {
                    attr.getContent(wr);
                });

                String attrVal = wr.toString();
                Html.escape(attrVal, writer, false, compact ? "\r\n" : null).append('"');
            }

        }

        if (!xhtml && this instanceof Closed) {
            writer.append("/>");
            return;
        }

        writer.append(">");

        int lineBreakCount = writer.getLineBreakCount();
        final TObject<Element> prev = new TObject<>();
        if (!children.isEmpty())

            writer.nextLevel(() -> {
                for (Element tag : children) {
                    if (!(prev.get() instanceof Text || tag instanceof Text))
                        writer.br();

                    writer.properties.put("prev-tag", prev.get());

                    tag.getContent(writer);
                    prev.set(tag);
                }
            });

        writeContentText(writer);

        if (!prev.isNull() && !(prev.get() instanceof Text) && lineBreakCount != writer.getLineBreakCount())
            writer.br().intent();

        writer.append("</").append(name).append(">");
    }

    // jesli dany tag tego wymaga, tutaj można przeciazyc metode zapisujaca tekst taga
    protected void writeContentText(StrWriter writer) {

        if (innerHtml != null)
            writer.append(innerHtml);

        if (innerAction != null)
            innerAction.getContent(writer);
    }

    public static class CustomTag extends CTag<CustomTag> {

        public CustomTag(Tag<? extends Parent> parent, String name) {
            super(parent, name);
        }

    }

}
