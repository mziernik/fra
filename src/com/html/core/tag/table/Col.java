package com.html.core.tag.table;

import com.html.core.tag.Element;
import com.html.core.tag.intfs.Closed;
import com.html.core.tag.intfs.Visual;

public class Col extends Element<Col> implements Visual<Col>, Closed {

    public Col(ColGroup parent) {
        super(parent, "col");
    }

    /**
     * Specifies the number of columns a <col> element should span
     *
     * @param span
     * @return
     */
    public Col span(Integer span) {
        return attrs.setNumber("span", span).tag;
    }

}
