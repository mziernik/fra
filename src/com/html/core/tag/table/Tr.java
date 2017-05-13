package com.html.core.tag.table;

import com.html.core.tag.Element;

public class Tr extends TableElement<Tr> {

    public Tr(TableElement parent) {
        super(parent, "tr");
    }

    public Td td() {
        return new Td(this);
    }

    public Td td(Object innerText) {
        return new Td(this).text(innerText);
    }

    public Th th() {
        return new Th(this);
    }

    public Th th(Object innerText) {
        return new Th(this).text(innerText);
    }

    public Tr setCells(Object... cells) {
        if (cells == null)
            return this;
        if (getParent() != null && getParent() instanceof THead)
            for (Object s : cells)
                th().text(s);
        else
            for (Object s : cells)
                td().text(s);
        return this;
    }

    public int getCellCount() {
        int cnt = 0;
        for (Element tag : this)
            if (tag.getName() != null
                    && (tag.getName().equals("td") || tag.getName().equals("th")))
                ++cnt;
        return cnt;
    }

    public int getIndex() {
        int idx = 0;
        for (Object o : getParentElement().getChildren()) {
            if (!(o instanceof Tr))
                continue;
            if (o == this)
                return idx;
            ++idx;
        }

        return -1;
    }

}
