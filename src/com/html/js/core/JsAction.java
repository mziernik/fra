package com.html.js.core;

import com.exceptions.ThrowableException;
import com.utils.text.StrWriter;
import com.html.core.Html;
import com.html.core.tag.Element;
import com.json.Escape;
import com.resources.core.html.ScriptFile;
import com.utils.text.WritableContent;
import java.io.IOException;
import com.utils.IUnquoted;

public abstract class JsAction implements WritableContent, IUnquoted {

    private Element tag;

    public final String getContent(boolean compact) {
        StrWriter writer = new StrWriter();
        if (compact)
            writer.setLineBreak(" ");
        getContent(writer);
        return writer.toString();
    }

    @Override
    public String toString() {
        return getContent(false);
    }

    protected StrWriter escape(StrWriter writer, Object value) {

        if (value instanceof IUnquoted) {
            writer.append(value.toString());
            return writer;
        }

        try {
            new Escape().singleQuota(true).toString(value, writer);
            return writer;
        } catch (IOException ex) {
            throw new ThrowableException(ex);
        }
    }

    /**
     * Należy tą metodę przeciążyć wszędzie tam, gdzie wykorzystywane są inne
     * akcje i należy wywołać tą metodę dla każdej z nich
     *
     * @param node
     * @return
     */
    public JsAction setTag(Element node) {
        this.tag = node;
        return this;
    }

    public Element getTag() {
        return tag;
    }

    protected void link(Element tag, ScriptFile... files) {
        if (tag == null)
            return;
        Html html = tag.getHTML();
        if (html == null)
            return;

        html.head.link(files);
    }

}
