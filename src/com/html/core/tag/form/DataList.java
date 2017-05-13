package com.html.core.tag.form;

import com.html.core.tag.Element;
import com.html.core.tag.form.input.Input;
import com.html.core.tag.intfs.Parent;
import java.util.Collection;

public class DataList extends Element<DataList> implements Parent<DataList> {

    public DataList(Parent parent, Input input) {
        super(parent, "datalist");
        if (input != null)
            input.list(this);
    }

    public DataList(Parent parent) {
        this(parent, null);
    }

    public Option option() {
        return new Option(this);
    }

    public DataList option(String value, Object text) {
        new Option(this).value(value).text(text);
        return self;
    }

    public DataList addAll(Collection<String> values) {
        if (values != null)
            for (String s : values)
                option().value(s);
        return this;
    }

}
