package com.html.bootstrap;

import com.html.core.tag.Tag;
import com.html.bootstrap.intfs.BsElement;
import com.html.core.tag.Element;
import com.html.core.tag.intfs.*;
import com.html.core.tag.semantic.Div;

/**
 * @author Miłosz Ziernik
 * @date 21 września 2015
 * @encoding UTF-8
 */
public class BsPanelGroups implements BsElement {

    Element parent;
    Div dPanelGroup;

    public <T extends Element & Parent & Visual> T getParent() {
        return (T) parent;
    }

    public BsPanelGroups(Tag<? extends Parent> parent) {
        //  this.parent = parent;
        dPanelGroup = new Div(parent.getElement());
        dPanelGroup.cls("panel-group");
    }

    public Div addPanel(String title, boolean expanded) {
        Div pnl = dPanelGroup.div().cls("panel panel-default");

        Div dHeader = pnl.div().cls("panel-heading");

        Div dBody = pnl.div().cls("panel-collapse collapse" + (expanded ? " in" : ""));

        dHeader.h4().cls("panel-title")
                .a(title)
                .aria("expanded", "false")
                .data("toggle", "collapse")
                .data("parent", "#" + dPanelGroup.getId(true))
                .href("#" + dBody.getId(true));

        return dBody.div().cls("panel-body");
    }

}
