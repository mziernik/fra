package com.html.core.tag.table;

import com.html.core.tag.intfs.Container;

public class Td extends TableElement<Td> implements Container<Td> {

    public Td(Tr parent) {
        super(parent, "td");
    }

    @Override
    public Td colspan(Integer colspan) {
        return super.colspan(colspan);
    }

    @Override
    public Td headers(String headers) {
        return super.headers(headers);
    }

    @Override
    public Td rowspan(Integer rowspan) {
        return super.rowspan(rowspan);
    }

    @Override
    public Tr getParent() {
        return (Tr) parent;
    }

}
