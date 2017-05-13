package com.html.bootstrap.panel;

import com.utils.Utils;
import com.utils.Is;
import com.html.bootstrap.intfs.BsElement;
import com.html.core.HtmlAttributes;
import com.html.core.tag.Tag;
import com.html.core.tag.intfs.BuildListener;
import com.html.core.tag.intfs.Parent;
import com.html.core.tag.semantic.Div;

/**
 * @author Miłosz Ziernik
 * @date 30 września 2015
 * @encoding UTF-8
 */
public class BsPanel implements BsElement, Tag<Div> {

    public final Div pnl;
    private Div header;
    private Div footer;
    public final Div body;

    private BsPanelType type;

    @Override
    public Div getElement() {
        return body;
    }

    public BsPanel type(BsPanelType type) {
        this.type = type;
        return this;
    }

    public Div header() {
        header.clear();
        header.cls("panel-heading");
        //   header.h4(title).cls("panel-title");
        return header;
    }

    public Div footer() {
        footer.clear();
        footer.cls("panel-footer");
        return footer;
    }

    public BsPanel(Tag<? extends Parent> parent) {
        type = Utils.coalesce(type, BsPanelType.default_);
        pnl = new Div(parent.getElement());
        pnl.cls("panel");

        if (header == null)
            header = pnl.div();

        body = pnl.div().cls("panel-body");

        if (footer == null)
            footer = pnl.div();
        /*
         if (expanded != null)
         body.cls("panel-collapse collapse" + (expanded ? " in" : ""));
         */
        pnl.node.addBuildListener(new BuildListener<Div>() {

            @Override
            public boolean onBeforeBuildTag(Div tag) {

                if (type != null)
                    pnl.cls("panel-" + type);
                return true;

            }
        });

    }

}
