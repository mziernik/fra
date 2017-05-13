package com.html.js;

import com.html.js.core.JsAction;
import com.json.Escape;
import com.utils.IUnquoted;
import com.utils.text.StrWriter;
import java.util.LinkedList;
import java.util.List;

/**
 * Dowolny wykonywalny kod. Klasa pełni rolę writera
 *
 * @author User
 */
public class Eval extends JsAction implements IUnquoted {

    private final List<Object> elements = new LinkedList<>();

    public Eval(Object script) {
        if (script != null)
            elements.add(script);
    }

    @Override
    public void getContent(StrWriter writer) {

        boolean first = true;
        for (Object obj : elements) {
            if (obj == null)
                continue;

            if (!first)
                writer.append(" ");

            if (obj instanceof JsAction)
                ((JsAction) obj).getContent(writer);
            else
                writer.append(obj);

            first = false;

        }
    }

    public Eval escape(Object value) {
        elements.add(new Escape().singleQuota(true).toString(value));
        return this;
    }

    public Eval append(JsAction action) {
        elements.add(action);
        return this;
    }

    public Eval append(String value) {
        elements.add(value);
        return this;
    }

}
