package com.html.bootstrap.form;

import com.html.core.tag.Tag;
import com.exceptions.ErrorMessage;
import com.html.bootstrap.SweetAlert;
import com.html.core.Html;
import com.html.core.dict.InputType;
import com.html.core.dict.MethodType;
import com.html.core.styles.FontWeight;
import com.html.core.tag.Element;
import com.html.core.tag.form.Form;
import com.html.core.tag.form.FormTag;
import com.html.core.tag.form.input.Label;
import com.html.core.tag.intfs.*;
import com.html.core.tag.semantic.CTag;
import com.html.core.tag.semantic.Div;
import com.html.core.tag.table.Table;
import com.html.core.tag.table.Tr;
import com.html.js.Eval;
import com.html.js.OnLoadDoc;
import com.json.*;
import com.mlogger.Log;
import com.resources.Res;
import com.servlet.Handlers;
import com.servlet.controller.Controller;
import com.servlet.requests.HttpRequest;
import com.servlet.requests.RequestParams;
import java.util.*;

/**
 * @author Miłosz Ziernik
 * @date 10 września 2015
 * @encoding UTF-8
 */
public abstract class BsFormController implements Controller, Tag<Form> {

    public final RequestParams params;
    private final HttpRequest http;
    private final List<BsFormGroup> groups = new LinkedList<>();
    private final Form form;

    protected abstract void formEvent(final HttpRequest req, final FormEvent event)
            throws Exception;

    /**
     * Konstruktor wywoływany automayucznie w momencie obsługi żądania
     *
     * @param req
     */
    public BsFormController(HttpRequest req) {
        form = null;
        this.http = req;
        params = req != null ? req.params : null;
    }

    protected boolean isFormEventRequest(HttpRequest req) {
        return req != null
                && req.params.has("event")
                && "application/javascript".equals(req.contentType());
    }

    /**
     * Konstruktor wykorzystywany podczas budowania HTML-a
     *
     * @param parent
     */
    public BsFormController(Tag<? extends Parent> parent) {
        Html html = ((Element) parent.getElement()).getHTML();
        if (html.getController() == null)
            html.setController(this);
        http = this.http();
        params = http.params;
        form = new Form(parent.getElement());

        if (isFormEventRequest(http))
            return;

        form.method(MethodType.post);
        form.attr("role", "form");

        html.head.link(Res.utils);
        html.head.link("/res/bootstrap/bsutils.js");

        if (!html.head.properties.containsKey("form-group-required")) {
            html.head.properties.put("form-group-required", true);
            html.head.styles("label[data-required]:before")
                    .content("\\2a")
                    .fontSize("0.75em")
                    .fontWeight(FontWeight._400)
                    .fontFamily("'Glyphicons Halflings'")
                    .marginRight("0.4em")
                    .color("#a94442");
        }

        form.node.addBuildListener((Form tag) -> {

            JArray arr = new JArray();
            arr.options.compactMode(true);
            for (BsFormGroup group : groups)
                group.onBeforeBuild(this, arr.array());
            tag.data("bsgroups", arr.toString());
            return true;
        });

        form.node.addBuildListener(new BuildListener<Form>() {

            @Override
            public boolean onBeforeBuildTag(Form tag) {
                form.onSubmit(new Eval("return " + new BsFormAction(BsFormController.this, "submit", null).toString()));
                form.action("#");
                form.script(new OnLoadDoc(new BsFormAction(BsFormController.this, "change", null)));
                return true;
            }
        });
    }

    public <T extends FormTag & Visual> BsFormController defineGroup(CTag group, T input, Label label) {

        group.cls("form-group");

        InputType type = (InputType) ((Element) input).attrs.getValue("type");

        if (type != InputType.radio && type != InputType.checkbox)
            input.cls("form-control");

        if (label != null)
            label.cls("control-label");

        groups.add(new BsFormGroup(group, input, label));

        ((FormTag) input).getName(true);

        return this;
    }

    @Override
    public void onRequest(HttpRequest req) throws Exception {

        String event = req.params.getStr("event");

        JElement el = JSON.parse(req.getInputStream());
        JArray src = el.asArray();

        List<FormField> lst = new LinkedList<>();
        for (JArray arr : src.getArrays()) {
            if (arr.size() != 5)
                continue;
            lst.add(new FormField(arr, req));
        }

        FormEvent frmEvent = new FormEvent(this, event,
                req.params.getStr("sender"),
                lst.toArray(new FormField[lst.size()]));

        try {
            formEvent(req, frmEvent);
        } catch (Throwable e) {
            Log.error(e);
            frmEvent.correct = true;

            ErrorMessage err = Handlers.errors.getInstance().getMessage(e);

            frmEvent.evalBefore.add(
                    new SweetAlert(SweetAlert.AlertType.error, err.message)
                            .title(err.title)
            );

        }

        req.returnJson(frmEvent.getJson());
    }

    @Override
    public Form getElement() {
        return form;
    }

    public <T extends FormTag & Visual> T addRow(Class<? extends T> cls,
            Table tbl, String label) {
        Tr tr = tbl.tbodyTr();
        Label lbl = new Label(tr.td(), null).text(label);
        T input = tr.td().div().tag(cls);
        lbl.for_(input);
        defineGroup((Div) input.getParent(), input, lbl);
        return input;
    }

}
