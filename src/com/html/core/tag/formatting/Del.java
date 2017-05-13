package com.html.core.tag.formatting;

import com.html.core.*;
import com.html.core.tag.intfs.*;
import com.html.core.tag.semantic.CTag;
import java.util.Date;

public class Del extends CTag<Del> {

    public Del(Parent parent) {
        super(parent, "del");
    }

    public Del date(Date date) {
        return attrs.set("date", date != null ? date.toString() : null).tag;
    }

}
