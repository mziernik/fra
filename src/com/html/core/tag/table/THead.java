package com.html.core.tag.table;

import com.html.core.tag.Element;
import com.html.core.tag.intfs.Parent;

public class THead extends TableElement<THead> {

    THead(Table parent) {
        super(parent, "thead");
    }

    @Override
    public Tr tr() {
        return super.tr();
    }

    @Override
    public Table getParent() {
        return (Table) super.getParent();
    }

}
