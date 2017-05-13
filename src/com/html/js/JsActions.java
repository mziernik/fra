package com.html.js;

import com.html.js.core.JsAction;
import com.utils.text.StrWriter;
import com.html.core.tag.Element;
import java.util.*;

/**
 * Zbiór wielu akcji
 *
 * @author User
 */
public class JsActions extends JsAction {
//ToDo: Zbadać wariant ze średnikami na końcu akcji (metody voidowe oraz inne zwracające dane)

    private final List<JsAction> actions = new LinkedList<>();
    private Element tag;

    public JsActions(JsAction... actions) {
        if (actions != null)
            for (JsAction act : actions)
                if (act != null)
                    this.actions.add(act);
    }

    public JsActions add(JsAction... actions) {
        if (actions != null)
            for (JsAction act : actions)
                if (act != null) {
                    this.actions.add(act);
                    if (tag != null)
                        act.setTag(tag);
                }
        return this;
    }

    public List<JsAction> getActions() {
        return actions;
    }

    public boolean isEmpty() {
        return actions.isEmpty();
    }

    public JsAction clear() {
        actions.clear();
        return this;
    }

    @Override
    public void getContent(StrWriter writer) {
        JsAction[] acts = actions.toArray(new JsAction[0]);

        for (int i = 0; i < acts.length; i++) {
            if (i > 0)
                writer.lineBreak(" ");

            acts[i].getContent(writer);

            if (i < acts.length - 1 && acts[i].getClass() != Eval.class)
                writer.append(";");
        }
    }

    @Override
    public JsAction setTag(Element tag) {
        this.tag = tag;
        for (JsAction act : actions)
            act.setTag(tag);
        return this;
    }

}
