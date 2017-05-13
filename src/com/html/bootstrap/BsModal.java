package com.html.bootstrap;

import com.html.core.tag.Tag;
import com.html.core.dict.ButtonType;
import com.html.core.tag.intfs.*;
import com.html.core.tag.semantic.Div;
import com.html.core.tag.semantic.H4;
import com.html.js.Call;
import com.html.js.core.JsAction;
import com.resources.Res;

/**
 * @author Miłosz Ziernik
 * @date 17 września 2015
 * @encoding UTF-8
 */
public class BsModal implements Tag<Div> {

    public final Div dHeader;
    public final Div dBody;
    public final Div dFooter;
    public final H4 dTitle;
    public final Div base;
    public Div dialog;

    public BsModal(Tag<? extends Parent> parent, String title) {

        parent.getHTML().head.link(Res.bootstrap);

        base = new Div(parent.getElement());

        base.cls("modal fade")
                .attr("role", "dialog")
                .aria("labelledby", "exampleModalLabel");

        dialog = base.div().cls("modal-dialog")
                .attr("role", "document");

        Div dContent = dialog.div().cls("modal-content");

        dHeader = dContent.div().cls("modal-header");

        dHeader.button()
                .type(ButtonType.button)
                .cls("close")
                .data("dismiss", "modal")
                .aria("label", "Zamknij")
                .span()
                .aria("hidden", "true")
                .innerHTML("&times;");

        dTitle = dHeader.h4(title)
                .cls("modal-title");

        dBody = dContent.div().cls("modal-body");

        //----------
        dFooter = dContent.div().cls("modal-footer");

        // ---------------------
        /* dFooter.button()
         .type(ButtonType.button)
         .cls("btn btn-default")
         .data("dismiss", "modal");
         */
    }

    /**
     * Przypisuje do zewnętrznego taga zdarzenie okClick, które wyświetla okno
     * modalne
     *
     * @param element
     * @return
     */
    public JsAction getShowAction() {
        return new Call("$", "#" + base.getId(true)).call("modal");
    }

    @Override
    public Div getElement() {
        return dBody;
    }
}
