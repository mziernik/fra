package com.html.core.tag.form.input;

import com.html.core.dict.EnctypeType;
import com.html.core.dict.InputType;
import com.html.core.tag.form.Select;
import com.html.core.tag.intfs.Parent;

public class InputFile extends Input<InputFile> {

    public InputFile(Parent parent) {
        super(parent, InputType.file);
        if (attrs.get("formenctype") == null)
            formEncType(EnctypeType.multipartFormData);
    }

    /**
     * Specifies the types of files that the server accepts (only for
     * type="file")
     *
     * @param accept
     * @return
     *
     * file_extension audio/* video/* image/* media_type
     */
    public InputFile accept(String accept) {
        return attrs.set("accept", accept).tag;
    }

    public InputFile multiple(boolean multiple) {
        return attrs.setState("multiple", multiple).tag;
    }

    @Override
    public InputFile required(boolean required) {
        return super.required(required);
    }
}
