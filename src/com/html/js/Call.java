package com.html.js;

import com.exceptions.ThrowableException;
import com.utils.text.StrWriter;
import com.html.js.core.JsAction;
import com.json.JCollection;
import java.io.IOException;
import java.util.*;

/**
 * Wywołanie metody z parametrami
 *
 * @author User
 */
public class Call extends JsAction implements Appendable {

    protected final List<Object> params = new LinkedList<>();
    protected String name;
    protected Call child;
    protected Call parent;

    private final StrWriter writer = new StrWriter();

    public Call(String name, Object... params) {
        if (params != null)
            this.params.addAll(Arrays.asList(params));
        this.name = name;
    }

    public Call param(Object value) {
        params.add(value);
        return this;
    }

    public boolean isAnonymous() {
        return name == null || name.trim().isEmpty();
    }

    public Call call(String name, Object... params) {
        child = new Call(name, params);
        child.parent = this;
        return child;
    }

    @Override
    public void getContent(StrWriter writer) {

        if (parent != null) {
            parent.getContent(writer);
            writer.append(".");
        }

        writer.append(name).append("(");

        boolean first = true;

        for (Object value : params) {
            if (!first)
                writer.append(", ");
            first = false;

            if (value instanceof JCollection) {
                JCollection json = (JCollection) value;
                json.options.singleLine(true).javascriptMode(true);
                try {
                    json.write(writer);
                } catch (IOException ex) {
                    throw new ThrowableException(ex);
                }
            } else if (value instanceof JsAction)
                ((JsAction) value).getContent(writer);
            else
                escape(writer, value);
        }

        writer.append(")");
        if (child == null)
            writer.append(this.writer.toString());
    }

    @Override
    public Call append(CharSequence csq) {
        writer.append(csq);
        return this;
    }

    @Override
    public Call append(CharSequence csq, int start, int end) {
        writer.append(csq, start, end);
        return this;
    }

    @Override
    public Call append(char c) {
        writer.append(c);
        return this;
    }
}
