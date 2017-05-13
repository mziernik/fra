package com.html.core.tag.table;

import com.html.core.tag.Element;
import com.html.core.tag.intfs.Parent;
import com.html.core.tag.intfs.Visual;

/*
 Contains

 The following element may appear only as the first one inside table:

 caption may appear at most once

 Either one or the other or neither of the following two elements may then appear:

 col may appear any number of times or not at all
 colgroup may appear any number of times or not at all

 Finally, one or more of the following elements must then appear in the order listed:

 thead may appear at most once, and only if tbody appears
 tfoot may appear at most once, and only if tbody appears
 tbody must appear at least once if, and only if, tr does not appear
 tr must appear at least once if, and only if, tbody does not appear
 */
public class Table extends Element<Table>
        implements Parent<Table>, Visual<Table> {

    public Table(Parent parent) {
        super(parent, "table");
    }

    public THead thead() {
        THead thead = this.getChildren(THead.class).peek();
        if (thead == null)
            thead = new THead(this);
        return thead;
    }

    public TBody tbody(boolean addNew) {
        if (addNew)
            return new TBody(this);
        TBody tbody = this.getChildren(TBody.class).peek();
        if (tbody == null)
            tbody = new TBody(this);
        return tbody;
    }

    public TFoot tfoot() {
        TFoot tfoot = this.getChildren(TFoot.class).peek();
        if (tfoot == null)
            tfoot = new TFoot(this);
        return tfoot;
    }

    public ColGroup colGroup() {
        ColGroup colgroup = this.getChildren(ColGroup.class).peek();
        if (colgroup == null)
            colgroup = new ColGroup(this);
        return colgroup;
    }

    public Tr theadTr() {
        return thead().tr();
    }

    public Tr tfootTr() {
        return tfoot().tr();
    }

    public Tr tbodyTr() {
        return tbody(false).tr();
    }

    public Table sortable(boolean sortable) {
        return attrs.setState("sortable", sortable).tag;
    }

}
