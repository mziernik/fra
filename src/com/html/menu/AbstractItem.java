package com.html.menu;

import com.html.core.tag.Tag;

public abstract class AbstractItem {

    public boolean enabled = false;
    public boolean visible = true;
    public final GroupItem parent;
    private Tag tag;

    public AbstractItem(GroupItem parent) {
        this.parent = parent;
    }

    public void setTag(Tag tag) {
        this.tag = tag;
    }

    public Tag getTag() {
        return tag;
    }
}
