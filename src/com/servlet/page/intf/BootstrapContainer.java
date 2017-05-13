package com.servlet.page.intf;

import com.html.core.tag.intfs.Parent;
import com.html.core.tag.semantic.Div;

public class BootstrapContainer extends Div {

    public static BootstrapContainer getInstance(Parent parent) {
        return null;
    }

    public BootstrapContainer(Parent parent) {
        super(parent);

        cls("container");

    }

}
