package com.html.bootstrap.button;

import com.html.bootstrap.intfs.BsElement;
import com.html.core.tag.form.Button;
import com.html.core.tag.intfs.BuildListener;
import com.html.core.tag.intfs.Parent;
import com.resources.Res;

/**
 * @author Miłosz Ziernik
 * @date 17 września 2015
 * @encoding UTF-8
 */
public class BsButton extends Button implements BsElement {

    private final Parent parent;

    private BsButtonSize size;
    private BsButtonType type;

    public BsButton(Parent parent) {
        super(parent);
        this.parent = parent;
        parent.getHTML().head.link(Res.bootstrap);
        cls("btn");

        node.addBuildListener(new BuildListener<Button>() {

            @Override
            public boolean onBeforeBuildTag(Button tag) {
                cls("btn-" + (type != null ? type.name().replace("_", "") : "default"));

                if (size != null && size.toString() != null)
                    cls(size.toString());
                return true;
            }
        });
    }

    public BsButton size(BsButtonSize size) {
        this.size = size;
        return this;
    }

    public BsButton type(BsButtonType type) {
        this.type = type;
        return this;
    }

    @Override
    public BsButton text(Object innerText) {
        super.text(innerText);
        return this;
    }
}
