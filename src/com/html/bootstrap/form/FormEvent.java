package com.html.bootstrap.form;

import com.html.js.JsActions;
import com.json.JObject;
import com.utils.collections.Strings;
import java.util.Objects;

/**
 * @author Miłosz Ziernik
 * @date 24 września 2015
 * @encoding UTF-8
 */
public class FormEvent {

    public final String name;
    public final FormField[] fields;
    Boolean correct = true;
    public final FormField sender; // identyfikator formularza lub pola typu input
    public final JsActions evalBefore = new JsActions(); // akcje do wykonania przed redirectem
    public final JsActions evalAfter = new JsActions(); // akcje do wykonania na końcu procedure

    FormEvent(BsFormController form, String name, String sender, FormField[] fields) {
        this.name = name;
        this.fields = fields;
        evalBefore.setTag(form.getElement());
        evalAfter.setTag(form.getElement());
        this.sender = getById(sender);
    }

    public boolean hasErrors() {
        for (FormField field : fields)
            if (field.status == FieldStatus.error)
                return true;
        return false;
    }

    public boolean isSubmit() {
        return "submit".equals(name);
    }

    public boolean isChange() {
        return "change".equals(name);
    }

    public Strings getErrors() {
        Strings list = new Strings().unique(true);
        for (FormField field : fields)
            if (field.status == FieldStatus.error && field.statusMessage != null)
                list.add(field.statusMessage);
        return list;
    }

    public FormField getById(String id) {
        for (FormField ff : fields)
            if (ff.id.equals(id))
                return ff;
        return null;
    }

    public FormField getByName(String name) {
        for (FormField ff : fields)
            if (ff.name.equals(name))
                return ff;
        return null;
    }

    public FormField getByProperty(String name, String value) {
        for (FormField ff : fields)
            if (Objects.equals(ff.properties.get(name), value))
                return ff;
        return null;
    }

    JObject getJson() {

        JObject resp = new JObject();

        if (!evalBefore.isEmpty())
            resp.put("evalBefore", evalBefore.toString());

        if (!evalAfter.isEmpty())
            resp.put("evalAfter", evalAfter.toString());

        JObject jFields = resp.objectC("fields");

        boolean hasErrors = false;
        for (FormField field : fields) {
            hasErrors |= field.status == FieldStatus.error
                    && field.statusMessage != null
                    && !field.statusMessage.isEmpty();

            jFields.objectC("f_" + field.id)
                    .put("sts", field.status)
                    .put("stsmsg", field.statusMessage)
                    .put("disabled", field.disabled)
                    .put("required", field.required)
                    .put("val", field.value);
        }

        if (correct == null)
            correct = !hasErrors;

        resp.put("correct", correct);
        return resp;
    }

}
