package com.html.modules;

import com.html.core.tag.Tag;
import com.html.core.styles.WhiteSpace;
import com.html.core.tag.Head;
import com.html.core.tag.formatting.Code;
import com.html.core.tag.formatting.Pre;
import com.html.core.tag.intfs.*;

public class CodeHighliter implements Tag<Code> {

    private CodeHighliterStyle style = CodeHighliterStyle.visualStudio;
    private final Code tag;

    @Override
    public Code getElement() {
        return tag;
    }

    public static enum CodeHighliterStyle {

        visualStudio("vs");
        private String name;

        private CodeHighliterStyle(String name) {
            this.name = name;
        }

        public String getName() {
            return name;
        }

    }

    public void setStyle(CodeHighliterStyle style) {
        if (style != null)
            this.style = style;
    }

    /**
     *
     * @param page
     * @param parent
     * @param type xml
     */
    public CodeHighliter(Tag<? extends Parent> parent, String type, String text) {

        Pre pre = new Pre(parent.getElement());
        pre.style().whiteSpace(WhiteSpace.normal);

        this.tag = pre.code(text);
        this.tag.cls(type);

        Head head = pre.getHTML().head;

        head.link(
                "/res/highlight/" + style.getName() + ".css",
                "/res/highlight/highlight.pack.js"
        );

        if (head.properties.containsKey("hljs"))
            return;

        head.script("hljs.initHighlightingOnLoad()");
        head.properties.put("hljs", true);

    }

}
