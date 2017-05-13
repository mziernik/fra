package com.html.core.tag.form;

import com.html.core.dict.EnctypeType;
import com.html.core.dict.MethodType;
import com.html.core.tag.intfs.Container;
import com.html.core.tag.intfs.Parent;
import com.html.js.core.JsAction;
import com.servlet.controller.Controller;
import com.utils.Url;

/*
 Contains

 Block elements, except form, at any depth
 fieldset
 */
public class Form extends FormTag<Form> implements Container<Form> {

    public Form(Parent parent) {
        super(parent, "form");
        encType(EnctypeType.applicationXWWWFormURLEncoded);
    }

    @Override
    public Form action(Url action) {
        return super.action(action);
    }

    @Override
    public Url action(String url) {
        return super.action(url);
    }

    @Override
    public Url action(Class<? extends Controller> page) {
        return super.action(page);
    }

    @Override
    public Url action(Controller page) {
        return super.action(page);
    }

    @Override
    public Form autocomplete(Boolean autocomplete) {
        return super.autocomplete(autocomplete);
    }

    @Override
    public Form formEncType(EnctypeType formenEnctypeType) {
        return super.formEncType(formenEnctypeType);
    }

    public Form method(MethodType methodType) {
        return attrs.setEnum("method", methodType).tag;
    }

    public Form encType(EnctypeType enctypeType) {
        return attrs.setEnum("enctype", enctypeType).tag;
    }

    public Form noValidate(boolean noValidate) {
        return attrs.setState("novalidate", noValidate).tag;
    }

    @Override
    public Form target(String framename) {
        return super.target(framename);
    }

    //-------------- zdarzenia -----------------------
    public Form onBlur(JsAction... onBlur) {
        return attrs.setAct("onblur", onBlur).tag;
    }

    public Form onFocus(JsAction... onFocus) {
        return attrs.setAct("onfocus", onFocus).tag;
    }

    public Form onInput(JsAction... onInput) {
        return attrs.setAct("oninput", onInput).tag;
    }

    public Form onInvalid(JsAction... onInvalid) {
        return attrs.setAct("oninvalid", onInvalid).tag;
    }

    public Form onReset(JsAction... onReset) {
        return attrs.setAct("onreset", onReset).tag;
    }

    public Form onSearch(JsAction... onSearch) {
        return attrs.setAct("onsearch", onSearch).tag;
    }

    public Form onSelect(JsAction... onSelect) {
        return attrs.setAct("onselect", onSelect).tag;
    }

    public Form onSubmit(JsAction... onSubmit) {
        return attrs.setAct("onsubmit", onSubmit).tag;
    }
}
