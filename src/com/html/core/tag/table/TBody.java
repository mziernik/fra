package com.html.core.tag.table;

import com.html.core.tag.Element;
import java.util.Collections;
import java.util.Comparator;

public class TBody extends TableElement<TBody> {

    TBody(Table parent) {
        super(parent, "tbody");
    }

    @Override
    public Tr tr() {
        return super.tr();
    }

    @Override
    public Table getParent() {
        return (Table) super.getParent();
    }

    public int getRowsCount() {
        int cnt = 0;
        for (Element nd : this)
            if (nd instanceof Tr)
                ++cnt;
        return cnt;

    }

    /*
     public void sortRows(final Comparator<Tr> comparator) {
     Collections.sort(children, new Comparator<Node>() {
     @Override
     public int compare(Node o1, Node o2) {
     if (!(o1 instanceof Tr) || !(o2 instanceof Tr))
     return 0;
     return comparator.compare((Tr) o1, (Tr) o2);
     }
     });
     }
     */
}
