package com.resources.awesome;

import com.utils.text.StrWriter;
import com.html.core.tag.Element;
import com.html.core.tag.intfs.Parent;
import com.html.core.tag.intfs.Visual;

/**
 * @author Miłosz Ziernik
 * @date 02 września 2015
 * @encoding UTF-8
 */
public class AwesomeIcon extends Element<AwesomeIcon> implements Visual<AwesomeIcon> {

    public final Awesome icon;
    private FaSize size;
    private boolean fixedWidth;
    private boolean inverse;
    private boolean pulse;
    private boolean spin;

    private FaRotate rotate;
    private boolean flipHorizontal;
    private boolean flipVertical;

    public AwesomeIcon(Parent parent, Awesome icon, FaSize size) {
        super(parent, "span");
        this.icon = icon;
        this.size = size;
        cls("fa fa-" + icon.name);
    }

    public AwesomeIcon size(FaSize size) {
        this.size = size;
        return this;
    }

    public AwesomeIcon fixedWidth(boolean fixedWidth) {
        this.fixedWidth = fixedWidth;
        return this;
    }

    public AwesomeIcon spin(boolean spin) {
        this.spin = spin;
        return this;
    }

    public AwesomeIcon pulse(boolean pulse) {
        this.pulse = pulse;
        return this;
    }

    public AwesomeIcon rotate(FaRotate rotate) {
        this.rotate = rotate;
        return this;
    }

    public AwesomeIcon flipHorizontal(boolean flipHorizontal) {
        this.flipHorizontal = flipHorizontal;
        return this;
    }

    public AwesomeIcon flipVertical(boolean flipVertical) {
        this.flipVertical = flipVertical;
        return this;
    }

    public AwesomeIcon inverse(boolean inverse) {
        this.inverse = inverse;
        return this;
    }

    @Override
    public void getContent(StrWriter writer) {
        String ssize = size != null ? size.value : "lg";

        AwesomeStack stack = null;
        if (parent instanceof AwesomeStack)
            stack = (AwesomeStack) parent;

        cls((fixedWidth ? " fa-fw" : " fa-" + (stack != null ? "stack-" : "") + ssize)
                + (rotate != null ? " fa-rotate-" + rotate.value : "")
                + (flipHorizontal ? " fa-flip-horizontal" : "")
                + (flipVertical ? " fa-flip-vertical" : "")
                + (inverse ? " fa-inverse" : "")
                + (spin ? " fa-spin" : "")
                + (pulse ? " fa-pulse" : "")
        );

        super.getContent(writer);
    }

}
