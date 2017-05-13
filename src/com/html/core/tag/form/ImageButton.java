package com.html.core.tag.form;

import com.html.core.styles.VerticalAlign;
import com.html.core.tag.intfs.Parent;
import com.html.core.tag.intfs.Visual;
import com.html.core.tag.semantic.Span;
import com.resources.core.html.ImgFile;
import com.utils.Url;

public class ImageButton extends AbstractButton<ImageButton> {

    public final Visual image;
    public final Span label;

    public ImageButton(Parent parent, ImgFile img) {
        super(parent);
        image = img.createTag(this);
        label = span();
        image.style().verticalAlign(VerticalAlign.middle);
        label.style().verticalAlign(VerticalAlign.middle);
    }

    public ImageButton(Parent parent, Url imgSrc) {
        super(parent);
        image = img().src(imgSrc);
        label = span();
        image.style().verticalAlign(VerticalAlign.middle);
        label.style().verticalAlign(VerticalAlign.middle);
    }

    @Override
    @Deprecated
    public ImageButton text(Object innerText) {
        throw new UnsupportedOperationException();
    }

}
