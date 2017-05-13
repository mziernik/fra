package com.html.modules;

import com.html.core.styles.Display;
import com.html.core.tag.Element;
import com.html.core.tag.form.Form;
import com.html.core.tag.form.ImageButton;
import com.html.core.tag.semantic.Div;
import com.html.js.*;
import com.html.js.core.JsAction;
import com.servlet.controller.Page;
import com.resources.Res;
import com.resources.core.html.ImgFile;
import com.utils.Url;

@Deprecated
public class WindowLayer {

    private final Page page;
    private final Div data;
    private Div dButtons;
    public Form form;
    public final String id;
    public String caption;
    public String footer;
    public String buttonsFooter;
    public Url iconUrl;
    public Integer iconWidth;
    public Integer iconHeight;
    public String onLoad;
    public String onSubmit;
    public boolean reloadOnSubmit = true;

    public WindowLayer(Page page) {
        this.page = page;
        id = page.request.getHeader("Layer-Id");

        data = page.body.div();

        data.style().display(Display.none);
        data.id("xdata_" + id);
        form = new Form(page.body);
        form.id("xfrm_" + id);
        form.onSubmit(new Eval("return layer.submit(this)"));
        page.body.style()
                .fontSize("9pt")
                .fontFamilySans()
                .padding("12px 16px 4px 16px");

        page.useAbsolutePaths = true;

    }

    public boolean onBeforeReturnHtml(Element tag, int status) {

        data.attrs.set("caption", caption)
                .set("footer", footer)
                .setHref("iconUrl", iconUrl)
                .setNumber("iconWidth", iconWidth)
                .setNumber("iconHeight", iconHeight)
                .set("onLoad", onLoad)
                .set("onSubmit", onSubmit)
                .setBool("reloadOnSubmit", reloadOnSubmit);

        if (form != null)
            data.attrs.setId("formId", form);

        if (iconUrl != null && !iconUrl.isEmpty())
            page.body.style().padding("16px 16px 16px 4px");

        /*
         przenies na koniec
         LinkedList<Node> lst = data_odl.parentNode().tags();
         lst.remove(data_odl);
         lst.add(data_odl);
         */
        return true;
    }

    public ImageButton addButton(ImgFile icon, String id, String caption,
            final JsAction onClick) {
        if (dButtons == null)
            dButtons = data.div().id("xbuttons_" + this.id);

        return dButtons.imageButton(icon, caption)
                .id(id)
                .onClick(onClick);
    }

    public ImageButton addButton(ImgFile icon, String id, String caption,
            boolean includeFramePrefix, JsAction onClick) {
        if (dButtons == null)
            dButtons = data.div().id("xbuttons_" + this.id);

        ImageButton btn = dButtons.imageButton(icon, caption).id(id).onClick(onClick);

        if (onClick != null)
            btn.onClick(includeFramePrefix
                    ? new IFrameEval("fra_" + WindowLayer.this.id, onClick)
                    : onClick);
        return btn;
    }

    public void clearButtons() {
        dButtons.removeTags(null);
    }

    public void setIcon(Url url, int width, int height) {
        iconUrl = url;
        iconWidth = width;
        iconHeight = height;
    }

    public ImageButton addCloseButton(String id, String caption) {
        return addButton(Res.close16png, id, caption, true, new Call("layer.close"));
    }

    public ImageButton addCloseButton() {
        return addCloseButton(null, "Zamknij");
    }

    public ImageButton addSubmitButton(String id, String caption) {
        return addButton(Res.apply16png, id, caption, true, new Call("layer.submit"));
    }

    public ImageButton addSubmitButton() {
        return addSubmitButton(null, "Zastosuj");
    }
}
