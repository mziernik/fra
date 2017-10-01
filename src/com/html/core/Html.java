package com.html.core;

import com.utils.text.StrWriter;
import com.dev.Dev;
import com.html.core.tag.Element;
import com.html.core.tag.Body;
import com.html.core.tag.Head;
import com.html.core.tag.Text;
import com.html.core.tag.intfs.OneInstance;
import com.html.core.tag.intfs.Parent;
import com.servlet.controller.Controller;
import com.utils.Url;
import com.utils.collections.MapList;
import com.utils.collections.Strings;
import com.utils.collections.TList;
import java.util.*;

/*
 Contains

 The following elements must each appear exactly once, and in the order listed.

 head
 body
 */
public class Html extends Element<Html> implements Parent<Html>, OneInstance {

    public Boolean xHTML = null;
    public Boolean compact = null;

    public static String INTENT = "  ";

    public final Head head;
    public final Body body;

    private Controller controller;

    public Html(Controller controller) {
        super(null, "html");
        if (controller != null)
            controller.http().properties.put("$htmlBuilder$", this);
        head = new Head(this);
        body = new Body(this);
        this.controller = controller;
    }

    public Html() {
        this(null);
    }

    protected Html manifest(Url manifest) {
        return attrs.setHref("manifest", manifest).tag;
    }

    public Controller getController() {
        return controller;
    }

    public void setController(Controller controller) {
        this.controller = controller;
    }

    public void validate(boolean xHtml) {

        final MapList<String, Element> ids = new MapList<>();
        final List<Element<?>> innerTextParents = new LinkedList<>();

        new Object() {
            void visit(Element<?> tag) {
                String id = tag.getId(false);

                if (id != null && !id.isEmpty())
                    ids.add(id, tag);
                if (tag instanceof Parent)
                    for (Element t : (Parent<Element>) tag)
                        visit(t);

                if (tag instanceof Text)
                    innerTextParents.add(tag.getParent().getElement());
            }
        }.visit(this);

        for (Map.Entry<String, TList<Element>> en : ids)
            if (en.getValue().size() > 1) {
                Strings strs = new Strings();
                for (Element t : en.getValue())
                    strs.add(t.getPath(true).toString("/"));
                Dev.warning("HTML", "Atrybut id = '" + en.getKey() + "' nie jest unikalny\nPowtórzenia:\n" + strs.toString("\n"));
            }

        for (Element parent : innerTextParents)
            if (xHtml && parent instanceof Body)
                Dev.warning("HTML", "Tekst nie moze zawierac sie w body dla trybu xHtml.");

    }

    public void getContent(StrWriter writer, boolean xHtml, boolean compact) {

        writer.properties.put("xhtml", xHtml);
        if (compact)
            writer.setIntent("").setLineBreak(" ");

        if (xHtml) {
            attrs.set("xmlns", "http://www.w3.org/1999/xhtml");
            attrs.set("lang", "pl");
            writer.append("<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n");
            writer.append("<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Transitional//EN\" "
                    + "\"http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd\">\n");
        } else
            writer.append("<!doctype html>").br();

        getContent(writer);

    }

    public static String escape(String s, boolean innerText) {
        return escape(s, new StrWriter(), innerText, null).toString();
    }

    public static StrWriter escape(String s, StrWriter writer, boolean innerText, String escapeChars) {

        for (char c : s.toCharArray()) {
            if ((c < 32 && c != 10 && c != 13)
                    || (escapeChars != null && escapeChars.indexOf(c) >= 0)) {
                writer.append("&#x")
                        .append(Integer.toString(c, 16))
                        .append(";");
                continue;
            }

            if (c == '\n') {
                //Może powodować problemy
                writer.br().intent();
                continue;
            }

            if (innerText)
                switch (c) {
                    case '<':
                        writer.append("&lt;");
                        break;
                    case '>':
                        writer.append("&gt;");
                        break;
                    case '&':
                        writer.append("&amp;");
                        break;
                    default:
                        writer.append(c);
                        break;
                }
            else
                switch (c) {
                    case '"':
                        writer.append("&quot;");
                        break;
                    case '&':
                        writer.append("&amp;");
                        break;
                    default:
                        writer.append(c);
                        break;
                }

            /*
             if (breakWords && ((c >= '!' && c <= '/')
             || (c >= ':' && c <= '@')
             || (c >= '[' && c <= '`')
             || (c >= '{' && c <= '~')))
             sb.append(Node.shy); */
        }

        return writer;
    }

}
