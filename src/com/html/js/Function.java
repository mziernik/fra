package com.html.js;

import com.utils.text.StrWriter;

import com.html.js.core.JsAction;
import com.html.core.tag.Element;
import com.json.Escape;
import com.utils.collections.Strings;
import java.util.*;

/**
 * Deklaracja funkcji
 *
 * @author User
 */
public class Function extends JsAction {

    private final Strings params = new Strings();
    private final List<Object> body = new LinkedList<>();
    private final String name;

    public Function(String name, String... params) {
        if (params != null)
            for (String o : params)
                this.params.add(Escape.unquoted(o));
        this.name = name;
    }

    public Function param(String paramName) {
        this.params.add(Escape.unquoted(paramName));
        return this;
    }

    public Function body(JsAction... body) {
        if (body != null)
            this.body.addAll(Arrays.asList(body));
        return this;
    }

    public Function body(String body) {
        if (body != null)
            this.body.add(body);
        return this;
    }

    public boolean isAnonymous() {
        return name == null || name.trim().isEmpty();
    }

    @Override
    public void getContent(StrWriter writer) {

        if (body.size() == 1 && (body.get(0) instanceof JsAction)) {
            JsAction act = (JsAction) body.get(0);
            Function f = null;
            if (act instanceof Function)
                f = (Function) act;
            if (act instanceof JsActions) {
                JsActions acts = (JsActions) act;
                if (acts.getActions().size() == 1 && acts.getActions().get(0) instanceof Function)
                    f = (Function) acts.getActions().get(0);
            }

            if (f != null && !f.isAnonymous() && f.name != null && !f.name.isEmpty())
                writer.append(f.name);

        }

        writer.append("function")
                .append(isAnonymous() ? "" : " " + name)
                .append("(")
                .append(params.toString(", "))
                .append("){");

        writer.nextLevel(() -> {
            for (Object value : body) {
                writer.lineBreak().intent();

                if (value instanceof JsAction) {
                    ((JsAction) value).getContent(writer);
                    writer.write(";");
                } else
                    writer.text(value);
            }
        });

        if (!body.isEmpty())
            writer.lineBreak().intent();
        writer.append("}");
    }

    @Override
    public JsAction setTag(Element tag) {
        for (Object value : body)
            if (value instanceof JsAction)
                ((JsAction) value).setTag(tag);
        return this;
    }

}
