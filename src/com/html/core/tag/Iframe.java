package com.html.core.tag;

import com.html.core.*;
import com.html.core.dict.SandboxType;
import com.html.core.tag.intfs.*;
import com.utils.Url;

public class Iframe extends Element<Iframe>
        implements Visual<Iframe>, InnerText<Iframe> {

    public Iframe(Parent parent) {
        super(parent, "iframe");
    }

    @Override
    public Iframe width(Number width) {
        return super.width(width);
    }

    @Override
    public Iframe height(Number height) {
        return super.height(height);
    }

    @Override
    public Iframe name(String name) {
        return super.name(name);
    }

    /**
     * Enables an extra set of restrictions for the content in an
     *
     * @param sandboxTypeVarargs
     * @return
     */
    public Iframe sandbox(SandboxType... sandboxTypeVarargs) {
        String sandboxTypeConcat = "";
        if (sandboxTypeVarargs != null)
            for (SandboxType sandboxType : sandboxTypeVarargs)
                if (sandboxType != null && sandboxType.toString().equals("sandbox")) {
                    attrs.set("sandbox", sandboxType.name());
                    return self;
                } else if (sandboxType != null)
                    sandboxTypeConcat += " " + sandboxType.toString();
                else {
                    attrs.set("sandbox", null);
                    return self;
                }
        attrs.set("sandbox", sandboxTypeConcat.substring(1));
        return self;
    }

    @Override
    public final Iframe src(Url src) {
        return super.src(src);
    }

    @Override
    public Url srcB(String src) {
        return super.srcB(src);
    }

}
