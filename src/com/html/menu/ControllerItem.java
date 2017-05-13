package com.html.menu;

import com.servlet.controller.Controller;

public class ControllerItem extends Element {

    public final Class<? extends Controller> ctrl;

    public ControllerItem(GroupItem parent, Class<? extends Controller> ctrl) {
        super(parent);
        this.ctrl = ctrl;
    }

}
