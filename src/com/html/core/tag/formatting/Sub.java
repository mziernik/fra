package com.html.core.tag.formatting;

import com.html.core.tag.intfs.*;
import com.html.core.tag.semantic.CTag;

/**
 * The <sub> tag defines subscript text. Subscript text appears half a character
 * below the normal line, and is sometimes rendered in a smaller font. Subscript
 * text can be used for chemical formulas, like H2O.
 *
 * @author user
 */
public class Sub extends CTag<Sub> {

    public Sub(Parent parent) {
        super(parent, "sub");
    }

}
