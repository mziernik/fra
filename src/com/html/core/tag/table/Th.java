package com.html.core.tag.table;

import com.html.core.tag.intfs.Container;

public class Th extends TableElement<Th> implements Container<Th> {

    public Th(Tr parent) {
        super(parent, "th");
    }

    @Override
    public Th colspan(Integer colspan) {
        return super.colspan(colspan);
    }

    @Override
    public Th headers(String headers) {
        return super.headers(headers);
    }

    @Override
    public Th rowspan(Integer rowspan) {
        return super.rowspan(rowspan);
    }

    @Override
    public Tr getParent() {
        return (Tr) parent;
    }

}
