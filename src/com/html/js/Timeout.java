package com.html.js;

import com.html.js.core.JsAction;
import com.utils.collections.Pair;
import com.utils.collections.Pairs;
import com.utils.text.StrWriter;
import java.util.LinkedList;

public class Timeout extends JsAction {

    private final JsActions actions;
    private final int timeout;
    private final Pairs<String, Object> params = new Pairs<>();

    public Timeout(int timeout, JsAction... actions) {
        this.timeout = timeout;
        this.actions = new JsActions(actions);
    }

    public Timeout param(String name, Object value) {
        params.add(name, value);
        return this;
    }

    @Override
    public void getContent(StrWriter writer) {

        Function funct = new Function(null)
                .body(actions);

        Call call = new Call("setTimeout", funct, timeout);

        for (Pair<String, Object> par : params) {
            funct.param(par.first);
            call.param(par.second);
        }

        call.getContent(writer);
    }

}
