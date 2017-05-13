package com.html.core.tag.image;

import com.html.core.tag.Element;
import com.html.core.tag.intfs.InnerText;
import com.html.core.tag.intfs.Parent;
import com.html.core.tag.intfs.Visual;

public class Canvas extends Element<Canvas> implements Visual<Canvas>, InnerText<Canvas> {

    public Canvas(Parent parent) {
        super(parent, "canvas");
    }

    @Override
    public Canvas width(Number width) {
        return super.width(width);
    }

    @Override
    public Canvas height(Number height) {
        return super.height(height);
    }

}
