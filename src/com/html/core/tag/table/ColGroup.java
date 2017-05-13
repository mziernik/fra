package com.html.core.tag.table;

import com.html.core.tag.Element;
import com.html.core.tag.intfs.Parent;

public class ColGroup extends Element<ColGroup> implements Parent<ColGroup> {

    public ColGroup(Table parent) {
        super(parent, "colgroup");
    }

    public Col col() {
        return new Col(this);
    }

    /**
     * Specifies the number of columns a <col> element should span
     *
     * @param span
     * @return
     */
    public ColGroup span(Integer span) {
        return attrs.setNumber("span", span).tag;
    }

    @Override
    public Table getParent() {
        return (Table) super.getParent();
    }

}
