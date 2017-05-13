package com.html.core.tag.form;

import com.html.core.HtmlAttributes.HtmlAttr;
import com.html.core.HtmlAttributes.StrAttr;
import com.html.core.tag.Element;
import com.html.core.dict.*;
import com.html.core.tag.Tag;
import com.html.core.tag.form.input.Label;
import com.html.core.tag.intfs.Parent;
import com.html.js.core.JsAction;
import com.servlet.controller.Controller;
import com.utils.Url;

/**
 * Klasa bazowa dla elementów typu: form, input, button, textarea
 *
 * @author Miłosz Ziernik
 * @date 24 sierpnia 2015
 * @encoding UTF-8
 */
public abstract class FormTag<TTag extends FormTag> extends Element<TTag> {

    public FormTag(Parent parent, String name) {
        super(parent, name);
    }

    protected TTag action(Url action) {
        return attrs.setHref("action", action).tag;
    }

    protected Url action(String url) {
        return attrs.setHrefB("action", url);
    }

    protected Url action(Class<? extends Controller> page) {
        Url url = new Url(page);
        attrs.setHref("action", url);
        return url;
    }

    protected Url action(Controller page) {
        Url url = new Url(page);
        attrs.setHref("action", url);
        return url;
    }

    protected TTag checked(boolean checked) {
        return attrs.setState("checked", checked).tag;
    }

    public TTag onInvalid(JsAction... onChange) {
        return attrs.setAct("oninvalid", onChange).tag;
    }

    /**
     * Specifies whether an <input> element should have autocomplete enabled
     *
     * @param autocomplete
     * @return
     */
    protected TTag autocomplete(Boolean autocomplete) {
        return attrs.setOnOff("autocomplete", autocomplete).tag;
    }

    /**
     * Specifies that an <input> element should automatically get focus when the
     * page loads
     *
     * @param autofocus
     * @return
     */
    protected TTag autofocus(boolean autofocus) {
        return attrs.setState("autofocus", autofocus).tag;
    }

    @Override
    public final TTag disabled(boolean disabled) {
        return super.disabled(disabled);
    }

    @Override
    public TTag name(String name) {
        return super.name(name);
    }

    /**
     * Specifies the URL of the file that will process the input control when
     * the form is submitted (for type="submit" and type="image")
     *
     * @param formaction
     * @return
     */
    protected TTag formAction(Url formaction) {
        return attrs.setHref("formaction", formaction).tag;
    }

    protected TTag formEncType(EnctypeType formenEnctypeType) {
        return attrs.setEnum("formenctype", formenEnctypeType).tag;
    }

    protected TTag formMethod(MethodType formMethodType) {
        return attrs.setEnum("formmethod", formMethodType).tag;
    }

    protected TTag formNoValidate(String formNoValidate) {
        return attrs.set("formnovalidate", formNoValidate).tag;
    }

    protected TTag formTarget(TargetType formTargetType) {
        return attrs.setEnum("formtarget", formTargetType).tag;
    }

    protected TTag list(DataList list) {
        return attrs.setId("list", list).tag;
    }

    protected TTag placeholder(String placeholder) {
        return attrs.set("placeholder", placeholder).tag;
    }

    protected TTag required(boolean required) {
        return attrs.setState("required", required).tag;
    }

    protected TTag size(Integer size) {
        return attrs.setNumber("size", size).tag;
    }

    protected Label labelAfter(Object caption) {
        return new Label(getParent(), this).text(caption);
    }

    protected Label labelBefore(Object caption) {
        return new Label(getParent(), this)
                .text(caption)
                .moveBefore(this)
                .getElement();
    }

    public Label getLabel() {
        return (Label) properties.get("#label");
    }

    public String getName(boolean createIfNotExists) {
        HtmlAttr<?> attr = attrs.get("name");
        String name = attr != null && attr instanceof StrAttr
                ? ((StrAttr) attr).value : null;
        if (createIfNotExists && (name == null || name.trim().isEmpty())) {
            name = getId(true);
            attrs.set("name", name);
        }
        return name;
    }
}
