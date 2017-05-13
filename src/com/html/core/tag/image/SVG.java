package com.html.core.tag.image;

import com.html.core.tag.Element;
import com.html.core.tag.intfs.*;

public class SVG extends Element<SVG> implements Parent<SVG>, Visual<SVG> {

    public SVG(Parent parent, String viewBox) {
        super(parent, "svg");
        attr("xmlns", "http://www.w3.org/2000/svg");
        attr("viewBox", viewBox);
    }

    public SVG viewBox(String viewBox) {
        attr("viewBox", viewBox);
        return this;
    }
}
