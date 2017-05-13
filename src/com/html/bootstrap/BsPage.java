package com.html.bootstrap;

import com.html.core.tag.semantic.Div;
import com.resources.Res;
import com.servlet.controller.Page;

public abstract class BsPage extends Page {

    public final Div container;

    public BsPage() {
        super();

        link(Res.bootstrap, Res.sweetAlert);
        container = body.div();
        container.cls("container");
    }

}
