package com.html.menu;

import com.servlet.controller.Controller;
import java.util.LinkedList;

public class GroupItem extends Element {

    public final LinkedList<AbstractItem> children = new LinkedList<>();

    public GroupItem(GroupItem parent) {
        super(parent);
    }

    public GroupItem addGroup(String caption) {
        GroupItem item = new GroupItem(this);
        item.caption = caption;
        children.add(item);
        return item;
    }

    public GroupItem addGroup(int index, String caption) {
        GroupItem item = new GroupItem(this);
        item.caption = caption;
        children.add(index, item);
        return item;
    }

    public ControllerItem add(Class<? extends Controller> controller) {
        ControllerItem item = new ControllerItem(this, controller);
        children.add(item);
        return item;
    }

}
