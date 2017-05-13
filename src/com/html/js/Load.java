package com.html.js;

import com.html.js.core.AbstractUrlAction;
import com.utils.text.StrWriter;
import com.html.core.tag.Element;
import com.html.js.core.JsAction;
import com.json.Escape;
import com.resources.Res;
import com.servlet.controller.Controller;
import com.utils.Url;
import com.utils.collections.Strings;

public class Load extends AbstractUrlAction<Load> {

    private boolean newWindow;
    private final Strings confirms = new Strings();
    private final Strings prompts = new Strings();
    private boolean top;

    public Load(String url) {
        super(url);
        this.newWindow = false;
    }

    public Load(Url url) {
        super(url);
        this.newWindow = false;
    }

    public Load newWindow(boolean newWindow) {
        this.newWindow = newWindow;
        return this;
    }

    public Load confirm(String message) {
        confirms.add("if (!confirm(" + Escape.js(message) + ")) return");
        return this;
    }

    /**
     * Dodaje prefix "top." przed funkcją load. Przdatne jeśli wywołujemy z
     * ramki
     *
     * @param top
     * @return
     */
    public Load top(boolean top) {
        this.top = top;
        return this;
    }

    public Load prompt(String paramName, String message, String defaultValue) {
        prompts.add(new StrWriter().singleQuote(true)
                .append("var p = prompt(")
                .escape(message).append(", ")
                .escape(defaultValue).append(");\n")
                .append("if (p === null) return;")
                .append("url.add(")
                .escape(paramName)
                .append(", p);").toString()
        );
        return this;
    }

    public Load(Class<? extends Controller> page) {
        this(new Url(page));
    }

    public Load(Controller page) {
        this(new Url(page));
    }

    @Override
    public void getContent(StrWriter writer) {

        if (!prompts.isEmpty()) {
            writer.append("var url = new UrlBuilder('")
                    .append(getUrl().toString())
                    .append("');\n");

            for (String s : confirms)
                writer.append(s).append(writer.isCompact() ? " " : "\n");

            for (String s : prompts)
                writer.append(s).append(writer.isCompact() ? " " : "\n");

            writer.append(top ? "top." : "")
                    .append("load(url.toString(), ")
                    .append(newWindow)
                    .append(")");
        }

        for (String s : confirms)
            writer.append(s).append(writer.isCompact() ? " " : "\n");

        writer.append("load('")
                .append(getUrl().toString())
                .append("', ")
                .append(newWindow)
                .append(")");
    }

    @Override
    public JsAction setTag(Element tag) {
        link(tag, Res.utils);

        super.setTag(tag);

        return this;
    }

}
