package com.html.core.tag;

import com.utils.Utils;
import com.utils.Is;
import com.html.core.Html;
import com.html.core.tag.intfs.*;
import com.html.core.tag.programming.Script;
import com.utils.text.StrWriter;

public class Text extends Element<Text> implements Parent<Text> {

    String text;
    boolean cdata;

    public Text(InnerText parent, boolean override) {
        super((Element) parent, "#text");

        if (override)
            for (Tag tag : Utils.asList(getParent().getChildren(null)))
                if (tag != this && tag instanceof Text)
                    getParent().getChildren().remove(tag);
    }

    public Text setInnerText(Object text, boolean cdata) {
        this.text = Utils.toString(text);
        this.cdata = cdata;

        if (cdata && text != null)
            this.text = this.text.replace("]]>", "]]]]><![CDATA[>");
        return this;
    }

    public String getInnerText() {
        return text;
    }

    @Override
    public void getContent(StrWriter writer) {
        if (getInnerText() != null) {

            boolean script = getParentElement() instanceof ScriptTag;

            String escapeChars = (node.options.escapeReturn != null
                    ? node.options.escapeReturn : writer.isCompact()) ? "\r\n" : null;

            int level = writer.getLevel();

            if (!script)
                writer.setLevel(0);
            else
                writer.br().intent();

            if (cdata)
                writer.append("<![CDATA[").append(text).append("]]>");
            else
                Html.escape(text, writer, true, escapeChars);
            writer.setLevel(level);

            if (script)
                writer.br().intent(writer.getLevel() - 1);
        }
    }

}
