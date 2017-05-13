package com.html.modules;

import com.utils.reflections.Reflections;
import com.html.core.tag.intfs.Container;
import com.html.core.tag.semantic.Div;
import com.html.core.tag.table.*;
import com.html.js.core.JsAction;
import com.html.js.Load;
import com.resources.core.html.ImgFile;
import com.servlet.controller.Page;
import com.servlet.interfaces.Endpoint;

@Deprecated
public class TopBar {

    public final Page page;
    public final Table tbl;
    public final Tr tr;

    public TopBar(Page page, Container parent) {
        this.page = page;
        tbl = parent.table();
        tbl.cls("topBar gradient1");
        page.head.link("/res/styles.css");
        tr = tbl.tbody(false).tr();
    }

    public Td add(Class<? extends Page> cls, ImgFile img) {
        Endpoint ipage = cls.getAnnotation(Endpoint.class);

        if (!page.session.user.rights.has(cls))
            return null;

        String name = cls.getSimpleName();
        String href = cls.getSimpleName().toLowerCase();
        String hint = null;

        if (ipage != null) {
            if (ipage.url() != null && ipage.url().length > 0) {
                href = ipage.url()[0];
                if (href.isEmpty() && ipage.url().length > 1)
                    href = ipage.url()[1];
            }

            if (ipage.title() != null)
                name = ipage.title();

            if (ipage.description() != null)
                hint = ipage.description();
        }
        Td td = tr.td();

        if (img != null)
            td.img(img);

        td.span(name);
        td.onClick(new Load(href));
        td.title(hint);

        //  if (Reflections.classExtends(cls, page.getClass()))
        //       td.attrs.set("selected", "1");
        return td;
    }

    public Div add(String title, JsAction onClick, boolean bold) {
        Div div = tr.td().div();
        div.text(title);
        div.onClick(onClick);
        if (bold)
            div.attrs.set("selected", "1");
        return div;
    }
}
