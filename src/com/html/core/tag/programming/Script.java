package com.html.core.tag.programming;

import com.html.core.tag.Element;
import com.html.core.tag.intfs.*;
import com.html.js.JsActions;
import com.html.js.core.JsAction;
import com.utils.Url;
import java.nio.charset.Charset;

public class Script extends Element<Script> implements InnerText<Script>, ScriptTag {

    public Script(Parent parent) {
        super(parent, "script");
        type("application/javascript");
    }

    public Script content(JsAction... actions) {
        if (actions == null)
            return this;

        innerAction = actions.length == 1 ? actions[0]
                : new JsActions(actions);
        for (JsAction act : actions)
            act.setTag(this);
        return this;
    }

    /**
     * Specifies that the script is executed asynchronously (only for external
     * scripts)
     *
     * @param async
     * @return
     */
    public Script async(boolean async) {
        return attrs.setState("async", async).tag;
    }

    /**
     * Specifies the character encoding used in an external script file
     *
     * @param charset
     * @return
     */
    public Script charset(Charset charset) {
        return attrs.set("charset", charset != null ? charset.name() : null).tag;
    }

    /**
     * Specifies that the script is executed when the page has finished parsing
     * (only for external scripts)
     *
     * @param defer
     * @return
     */
    public Script defer(boolean defer) {
        return attrs.setState("defer", defer).tag;
    }

    @Override
    public Script src(Url src) {
        return super.src(src);
    }

    /**
     * Specifies the media type of the script
     *
     * @param type
     * @return
     */
    public Script type(String type) {
        return attrs.set("type", type).tag;
    }

}
