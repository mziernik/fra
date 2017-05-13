package com.html.core.tag.table;

import com.html.core.tag.Element;
import com.html.core.tag.intfs.Parent;
import com.html.core.tag.intfs.Visual;

public abstract class TableElement<TTag extends TableElement>
        extends Element<TTag>
        implements Parent<TTag>, Visual<TTag> {

    public TableElement(Parent parent, String name) {
        super(parent, name);
    }

    protected Tr tr() {
        return new Tr(this);
    }

    protected TTag colspan(Integer colspan) {
        return attrs.setNumber("colspan", colspan).tag;
    }

    protected TTag rowspan(Integer rowspan) {
        return attrs.setNumber("rowspan", rowspan).tag;
    }

    protected TTag headers(String headers) {
        return attrs.set("headers", headers).tag;
    }

}
