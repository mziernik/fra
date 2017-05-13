package com.html.core.tag;

import com.html.core.Html;
import com.html.core.tag.intfs.OneInstance;
import com.html.core.tag.semantic.CTag;
import com.html.js.core.JsAction;

public class Body extends CTag<Body> implements OneInstance {

    public Body(Html parent) {
        super(parent, "body");
    }

    public Body onBeforeUnload(JsAction... onBeforeUnload) {
        return attrs.setAct("onbeforeunload", onBeforeUnload).tag;
    }

    public Body onError(JsAction... onError) {
        return attrs.setAct("onerror", onError).tag;
    }

    public Body onHashChange(JsAction... onHashChange) {
        return attrs.setAct("onhashchange", onHashChange).tag;
    }

    public Body onLoad(JsAction... onLoad) {
        return attrs.setAct("onload", onLoad).tag;
    }

    public Body onResize(JsAction... onResize) {
        return attrs.setAct("onresize", onResize).tag;
    }

    public Body onUnload(JsAction... onUnload) {
        return attrs.setAct("onunload", onUnload).tag;
    }

}
