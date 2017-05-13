package com.html.menu;

import com.html.core.tag.Tag;
import com.resources.BsIcon;

public abstract class Element extends AbstractItem {

    public String caption;
    public BsIcon icon;

    public Element(GroupItem parent) {
        super(parent);
    }

}
