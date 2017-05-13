package com.html.bootstrap;

import com.utils.reflections.Reflections;
import com.servlet.controller.ControllerMetaData;
import com.html.bootstrap.form.BsFormController;
import com.html.core.Html;
import com.html.core.dict.ButtonType;
import com.html.core.tag.form.Form;
import com.html.core.tag.intfs.*;
import com.html.core.tag.semantic.Div;
import com.html.core.tag.semantic.H4;
import com.html.js.*;
import com.servlet.controller.Controller;
import com.servlet.requests.HttpRequest;
import com.utils.Url;
import com.utils.collections.Params;

/**
 * @author Miłosz Ziernik
 * @date 17 września 2015
 * @encoding UTF-8
 */
public abstract class BsModalForm extends BsFormController {

    protected final Div header;
    protected final Div body;
    protected final Div footer;
    protected final H4 title;
    public final Div base;
    public Div dialog;

    public BsModalForm(Controller controller) {
        super(controller.http());

        header = null;
        body = null;
        footer = null;
        base = null;
        title = null;
    }

    public BsModalForm(HttpRequest req) {
        super(new Html(req.controller).body);

        if (isFormEventRequest(req)) {
            header = null;
            body = null;
            footer = null;
            base = null;
            title = null;
            return;
        }

        base = getElement().getHTML().body.div();
        base.id(getId(getClass()));

        base.cls("modal fade")
                .attr("role", "dialog")
                .aria("labelledby", "exampleModalLabel");

        Form form = getElement();
        form.remove();
        base.node.add(form);

        dialog = form.div().cls("modal-dialog")
                .attr("role", "document");

        Div dContent = dialog.div().cls("modal-content");

        header = dContent.div().cls("modal-header");

        header.button()
                .type(ButtonType.button)
                .cls("close")
                .data("dismiss", "modal")
                .aria("label", "Zamknij")
                .span()
                .aria("hidden", "true")
                .innerHTML("&times;");

        title = header.h4()
                .cls("modal-title");

        body = dContent.div().cls("modal-body");
        footer = dContent.div().cls("modal-footer");

    }

    protected abstract void processRequest(HttpRequest req) throws Exception;

    @Override
    public final void onRequest(HttpRequest http) throws Exception {
        if (isFormEventRequest(http)) {
            super.onRequest(http);
            return;
        }
        processRequest(http);
        http.returnHTML(base, 200);
    }

    /**
     * Przypisuje do zewnętrznego taga zdarzenie okClick, które wyświetla okno
     * modalne
     *
     * @param element
     * @param params
     * @return
     */
    public BsModalForm assignOnClick(Visual element, Params params) {
        element.onClick(new Call("bsModalForm",
                getId(getClass()),
                new Url(getClass()).toString(),
                params.toJson()));
        return this;
    }

    public static String getId(Class<? extends BsModalForm> cls) {
        return "bs_modal_" + Reflections.getClassHash(cls);
    }

    public Call getCloseAction(Class<? extends BsModalForm> cls) {
        return new Call("$", "#" + getId(cls)).call("modal", "hide");
    }

}
