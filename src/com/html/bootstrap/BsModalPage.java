package com.html.bootstrap;

import com.exceptions.http.Http405MethodNotAllowed;
import com.html.core.Html;
import com.html.core.dict.ButtonType;
import com.html.core.tag.semantic.Div;
import com.html.core.tag.semantic.H4;
import com.json.Escape;
import com.resources.Res;
import com.servlet.controller.Controller;
import com.servlet.controller.ControllerMetaData;
import com.servlet.controller.ControllerEndpoint;
import com.servlet.controller.intf.AfterRequest;
import com.utils.reflections.TMethod;
import com.utils.text.StrWriter;

/**
 * @author Miłosz Ziernik
 * @date 17 września 2015
 * @encoding UTF-8
 */
public abstract class BsModalPage implements Controller {

    public final Div header;
    public final Div body;
    public final Div footer;
    public final H4 title;
    public final Div base;
    public Div dialog;

    private void afterProcessPage(ControllerEndpoint<?> ctrl, TMethod method,
            Throwable err) throws Http405MethodNotAllowed {

        if (err != null)
            return;

        if (!ctrl.http.isAjaxRequest || ctrl.http.isLocked())
            throw new Http405MethodNotAllowed("Żądanie nie zostało zainicjowane przez Ajax-a");

        StrWriter writer = new StrWriter();
        writer.append("var id = '").append(base.getId(true)).append("';\n")
                .append("var d = document.getElementById('pre-' + id);\n")
                .append("if (!d) d = document.createElement('div');\n")
                .append("d.setAttribute('id', 'pre-' + id);\n")
                .append("d.innerHTML = ")
                .append(new Escape().singleQuota(true).toString(base.toString()))
                .append(";\ndocument.body.appendChild(d);\n")
                .append("$('#' + id).modal();");

        ctrl.http.setHeader("X-Requested-Eval", "true");
        ctrl.http.returnCustom(writer.toString(), "application/javascript");
    }

    public BsModalPage() {
        super();

        endpoint().afterProcessRequest.add((AfterRequest) this::afterProcessPage);

        Html html = new Html(this);

        html.head.link(Res.bootstrap);

        base = html.body.div();
        base.id("modal-" + ControllerMetaData.getHash(this));

        base.cls("modal fade")
                .attr("role", "dialog")
                .aria("labelledby", "exampleModalLabel");

        dialog = base.div().cls("modal-dialog")
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

        title = header.h4(endpoint().title())
                .cls("modal-title");

        body = dContent.div().cls("modal-body");

        //----------
        footer = dContent.div().cls("modal-footer");

        // ---------------------
        /* footer.button()
         .type(ButtonType.button)
         .cls("btn btn-default")
         .data("dismiss", "modal");
         */
    }

}
