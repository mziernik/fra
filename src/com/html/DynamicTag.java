package com.html;

import com.utils.reflections.Reflections;
import com.html.core.Html;
import com.html.core.tag.Tag;
import com.html.core.tag.programming.Script;
import com.html.core.tag.semantic.CTag;
import com.html.core.tag.semantic.Div;
import com.html.js.*;
import com.resources.Res;
import com.servlet.controller.Page;

/**
 * @author Miłosz Ziernik
 * @date 24 września 2015
 * @encoding UTF-8
 */
public abstract class DynamicTag extends Page implements Tag<Div> {

    private final Div tag;
    private Script script;

    public DynamicTag(Tag<? extends CTag> parent) {
        Div tag = parent != null ? parent.getElement().div() : null;
        if (tag == null) {
            this.tag = null;
            return;
        }

        tag.id("dt-" + Reflections.getClassHash(getClass()));

        this.tag = tag;

        script = tag.script();
        Html html = tag.getHTML();
        if (html != null)
            html.head.link(Res.utils);

        tag.node.addBuildListener((Div tag1) -> {
            if (script != null)
                script.content(new OnLoadDoc(getLoadAction()));
            return true;
        });
    }

    @Override
    public void returnHTML() {
        http().setHeader("dt-tag-id", "dt-" + Reflections.getClassHash(getClass()));
        body.node.setName("div");
        request.returnHTML(body, 200);
    }

    public Ajax getLoadAction() {
        return new Ajax(getClass())
                .onDone(new Eval("$$service.loadDynContent(this);"));
    }

    @Override
    public Div getElement() {
        return tag;
    }

}
