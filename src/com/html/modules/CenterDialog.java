package com.html.modules;

import com.html.core.styles.*;
import com.html.core.tag.Body;
import com.html.core.tag.Element;
import com.html.core.tag.form.Form;
import com.html.core.tag.semantic.Div;
import com.html.core.tag.table.Table;
import com.html.core.tag.table.Th;
import com.html.js.Eval;
import com.servlet.controller.Page;

@Deprecated
public class CenterDialog {

    public final Form form;
    public final Div dError;
    public final Table table;
    public final Th header;

    public CenterDialog(Page page, String error, String btnApplyId) {

        Body body = page.body;

        page.head.styles(".ctdlg", ".ctdlg table", "#dError")
                .fontFamily("Arial")
                .fontSize("10pt")
                .margin("0")
                .textAlign(TextAlign.left);

        page.head.styles(".ctdlg input[type=text]",
                ".ctdlg input[type=password]",
                ".ctdlg input[type=email]")
                .padding("4px 8px")
                .borderRadius("4px")
                .fontSize("11pt")
                .border("1px solid #999")
                .backgroundColor("#fff")
                .boxShadow("0 0 2px #bbb")
                .margin("3px 0")
                .width("100%")
                .minWidth("200px");

        page.head.styles(".ctdlg input[type=submit]")
                .float_(SFloat.right)
                .marginTop("4px")
                .padding("4px 24px");

        page.head.styles(".ctdlg td:nth-child(1)")
                .textAlign(TextAlign.right)
                .paddingRight("8px");

        page.head.styles(".ctdlg td:nth-child(2)")
                .paddingRight("20px");

        page.head.styles(".ctdlg th")
                .fontSize("10pt")
                .textAlign(TextAlign.center)
                .color("#444");

        dError = body.div();
        dError.id("dError");
        dError.text(error);
        dError.style().fontSize("11pt")
                .display(error != null ? Display.block : Display.none)
                .position(Position.absolute)
                .left("10%")
                .right("10%")
                .top("7%")
                .padding("16px")
                .gradientBackground(null, "#ffe4e4", "#fbb")
                .border("2px solid #d66")
                .backgroundColor("#fdd")
                .textAlign(TextAlign.center)
                .borderRadius("6px")
                .boxShadow("2px 2px 4px #bbb");

        Div dCenter = body.div().id("dCenter");
        dCenter.style().position(Position.absolute)
                .left("50%")
                .top("50%");

        Div dLogin = dCenter.div();
        dLogin.id("dLogin");
        dLogin.style()
                .border("1px solid #999")
                .padding("12px 16px 4px 16px")
                .backgroundColor("#f4f4f4")
                .borderRadius("4px")
                .boxShadow("0 0 6px #cccccc")
                .gradientBackground(page.http().userAgent, "#ffffff", "#eeeeee");

        form = dLogin.form();
        form.cls("ctdlg");
        form.onSubmit(new Eval("document.getElementById('" + btnApplyId + "').disabled = true;"));

        table = form.table();

        header = table.theadTr().th();
        header.colspan(2);

        //  dLogin.style().padding("8px 16px 4px 16px");
        header.style().paddingBottom("8px")
                .borderBottom("1px solid #aaa");

        table.theadTr().th()
                .colspan(2)
                .style().height("4px");

        body.script(new Eval("   var tag = document.getElementById('dCenter');\n"
                + "   var d = document.getElementById('dError').style.display !== 'none' ? 4 : 2;   \n"
                + "   tag.style.marginTop = (-tag.offsetHeight / d) + 'px';\n"
                + "   tag.style.marginLeft = (-tag.offsetWidth / 2) + 'px';"));
    }

}
