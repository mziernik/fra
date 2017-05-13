package com.html.core.tag.list;

import com.html.core.tag.Element;
import com.html.core.dict.OlType;
import com.html.core.tag.intfs.Parent;
import com.html.core.tag.intfs.Visual;

public class Ol extends Element<Ol> implements Parent<Ol>, Visual<Ol> {

    public Ol(Parent parent) {
        super(parent, "ol");
    }

    public Ol reversed(boolean reversed) {
        return attrs.setState("reversed", reversed).tag;
    }

    public Ol start(Integer start) {
        return attrs.setNumber("start", start).tag;
    }

    /**
     * Specifies the kind of marker to use in the list
     *
     * @param type
     * @return
     */
    public Ol type(OlType type) {
        return attrs.setEnum("type", type).tag;
    }

    public Li li() {
        return new Li(this);
    }

    public Li li(Object innerText) {
        return new Li(this).text(innerText);
    }

}
