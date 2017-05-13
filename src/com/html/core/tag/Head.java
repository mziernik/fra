package com.html.core.tag;

import com.utils.Utils;
import com.utils.Is;
import com.context.index.Index;
import com.dev.Dev;
import com.html.core.*;
import com.html.core.HtmlAttributes.HrefAttr;
import com.html.core.HtmlAttributes.HtmlAttr;
import com.html.core.dict.HttpEquiv;
import com.html.core.dict.LinkType;
import com.html.core.tag.intfs.*;
import com.html.core.tag.meta.Base;
import com.html.core.tag.meta.Link;
import com.html.core.tag.meta.Meta;
import com.html.core.tag.programming.Script;
import com.html.js.core.JsAction;
import com.mlogger.Log;
import com.resources.core.IdxRes;
import com.resources.core.Resources;
import com.resources.core.html.ScriptFile;
import com.servlet.controller.Controller;
import com.utils.Url;
import com.utils.collections.MapList;
import java.io.FileNotFoundException;
import java.net.URL;
import java.util.*;
import java.util.Map.Entry;

public class Head extends Element<Head> implements Parent<Head>, OneInstance {

    public Head(Html parent) {
        super((Parent) parent, "head");
        meta().httpEquiv(HttpEquiv.contentType).content("text/html; charset=utf-8");
        requiredTag("title");
    }

    public Base base() {
        return new Base(this);
    }

    public Link link() {
        return new Link(this);
    }

    public Link link(Url href) {
        return new Link(this).href(href);
    }

    public Link linkCSS(String src) {

        if (!src.startsWith("/")) {
            Controller ctrl = getHTML().getController();
            if (ctrl != null) {
                String name = ctrl.getClass().getPackage().getName()
                        .replace(".", "/") + "/" + src;

                IdxRes idx = IdxRes.getByPath(name);
                if (idx != null)
                    return linkCSS(idx.getUrl());
                else
                    Dev.warning(new FileNotFoundException(src));
            }
        }

        return linkCSS(new Url(src));
    }

    public Link linkCSS(Url src) {
        return new Link(this)
                .rel(LinkType.stylesheet)
                .type("text/css")
                .href(src);
    }

    public Script linkJS(String href) {

        if (!href.startsWith("/")) {
            Controller ctrl = getHTML().getController();
            if (ctrl != null) {
                String name = ctrl.getClass().getPackage().getName()
                        .replace(".", "/") + "/" + href;
                IdxRes idx = IdxRes.getByPath(name);
                if (idx != null)
                    return linkJS(idx.getUrl());
                else
                    Dev.warning(new FileNotFoundException(href));
            }
        }

        return linkJS(new Url(href));
    }

    public Script linkJS(Url href) {
        return new Script(this)
                .src(href)
                .type("application/javascript");
    }

    public void link(ScriptFile... files) {
        if (files != null)
            for (ScriptFile f : files)
                f.link(this);
    }

    public void link(String... files) {
        if (files == null)
            return;

        for (String f : files) {
            String s = f.trim().toLowerCase();

            if (s.endsWith(".js"))
                linkJS(f);
            else if (s.endsWith(".css"))
                linkCSS(f);
        }
    }

    public Head icon(Url href) {
        Icon ico = getChildren(Icon.class).peek();
        if (ico == null)
            ico = new Icon();
        ico.href(href);
        return this;
    }

    public final Meta meta() {
        return new Meta(this);
    }

    public Script script() {
        return new Script(this);
    }

    public Script script(String content) {
        return new Script(this).text(content);
    }

    public Script script(JsAction... actions) {
        return new Script(this).content(actions);
    }

    public Script script(Url src) {
        return new Script(this).src(src);
    }

    public final Title title() {
        return getChildren(Title.class).peek();
    }

    public final Title title(Object innerText) {
        Title title = title();
        if (title == null)
            title = new Title();
        return title.text(innerText);
    }

    public Meta meta(String name, Object content) {
        return meta().name(name).content(Utils.toString(content));
    }

    public class Icon extends Link {

        public Icon() {
            super(Head.this);
            rel(LinkType.icon);
        }

    }

    public class Title extends Element<Title> implements InnerText<Title>, OneInstance {

        public Title() {
            super((Parent) Head.this, "title");

        }
    }

    public void optimize(boolean xHtml) {

        // ---------------------------- usun powtarzajace sie linki i skrypty --------------------
        MapList<String, Link> links = new MapList<>();
        MapList<String, Script> scripts = new MapList<>();

        for (Tag t : getChildren()) {

            if (t instanceof Link) {
                Link link = (Link) t;
                HtmlAttr<?> attr = link.attrs.get("href");
                if (attr != null && attr.value != null && attr instanceof HrefAttr)
                    links.add(((HrefAttr) attr).value.toString(), link);
            }

            if (t instanceof Script) {
                Script script = (Script) t;
                HtmlAttr<?> attr = script.attrs.get("src");
                if (attr != null && attr.value != null && attr instanceof HrefAttr)
                    scripts.add(((HrefAttr) attr).value.toString(), script);
            }

        }

        for (Entry<String, LinkedList<Link>> en : links)
            if (en.getValue().size() > 1) {
                Iterator<Link> itr = en.getValue().iterator();
                itr.next(); // pomin pierwszy
                while (itr.hasNext())
                    itr.next().remove();
            }

        for (Entry<String, LinkedList<Script>> en : scripts)
            if (en.getValue().size() > 1) {
                Iterator<Script> itr = en.getValue().iterator();
                itr.next(); // pomin pierwszy
                while (itr.hasNext())
                    itr.next().remove();
            }

    }

}
