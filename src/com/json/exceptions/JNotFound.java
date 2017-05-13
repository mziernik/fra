package com.json.exceptions;

import com.json.Escape;
import com.json.*;
import com.lang.LJson;

public class JNotFound extends JException {

    public JNotFound(JCollection parent, String name) {
        super(LJson.ELEMENT_NOT_FOUND.toString(parent.getPath()
                .add(Escape.escape(name)).toString()));

    }

    public JNotFound(JCollection parent, int index) {
        super(LJson.ELEMENT_NOT_FOUND.toString(parent.getPath()
                .add("[" + index + "]").toString()));
    }
}
