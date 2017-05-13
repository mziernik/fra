package com.html.bootstrap.form;

import com.utils.text.StrWriter;
import com.servlet.controller.ControllerMetaData;
import com.html.core.tag.form.Form;
import com.html.core.tag.form.FormTag;
import com.html.js.core.JsAction;
import com.json.JObject;
import com.utils.Url;

/**
 * @author Miłosz Ziernik
 * @date 24 września 2015
 * @encoding UTF-8
 */
public class BsFormAction extends JsAction {

    private final JObject params;

    public BsFormAction(BsFormController ctrl, String event, FormTag input) {
        Form frm = ctrl.getElement();

        frm.data("action", frm.getHTML().getController().http().getRelativePath(new Url(ctrl.getClass())));
        params = new JObject()
                .put("referer", frm.getHTML().getController().http().url.toString())
                .put("event", event);

        if (input != null)
            params.put("inputId", input.getId(true));
        else
            params.put("formId", frm.getId(true));

        params.options.javascriptMode(true).compactMode(true);
    }

    @Override
    public void getContent(StrWriter writer) {
        writer.append("bsFormEvent(").append(params.toString()).append(");");
    }

}
