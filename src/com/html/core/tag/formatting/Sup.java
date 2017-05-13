package com.html.core.tag.formatting;

import com.html.core.*;
import com.html.core.tag.intfs.*;
import com.html.core.tag.semantic.CTag;

/**
 * The <sup> tag defines superscript text. Superscript text appears half a
 * character above the normal line, and is sometimes rendered in a smaller font.
 * Superscript text can be used for footnotes, like WWW[1].
 *
 * @author user
 */
public class Sup extends CTag<Sup> {

    public Sup(Parent parent) {
        super(parent, "sup");
    }

}
