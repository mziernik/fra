package com.html.bootstrap.form;

import com.html.core.tag.Element;
import com.html.core.tag.form.FormTag;
import com.html.core.tag.form.input.Label;
import com.json.JArray;

/**
 * @author Miłosz Ziernik
 * @date 24 września 2015
 * @encoding UTF-8
 */
class BsFormGroup {

    final Element group;
    final FormTag input;
    final Label label;

    public BsFormGroup(Element group, FormTag input, Label label) {
        this.group = group;
        this.input = input;
        this.label = label;
    }

    void onBeforeBuild(BsFormController form, JArray array) {
        array.addAll(group != null ? group.getId(true) : null,
                input != null ? input.getId(true) : null,
                label != null ? label.getId(true) : null);

        if (input != null)
            input.onChange(new BsFormAction(form, "change", input));

        if (input != null && label != null && input.attrs.has("required"))
            label.data("required", "");
    }

}
