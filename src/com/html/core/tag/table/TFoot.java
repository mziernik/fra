package com.html.core.tag.table;

import com.html.core.tag.Element;

public class TFoot extends TableElement<TFoot> {

    TFoot(Table parent) {
        super(parent, "tfoot");
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
