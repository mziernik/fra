package com.html.core.tag.formatting;

import com.html.core.tag.intfs.Parent;
import com.html.core.tag.semantic.CTag;

/**
 * @author Miłosz Ziernik
 * @date 26 sierpnia 2015
 * @encoding UTF-8
 */
public class Blockquote extends CTag<Blockquote> {

    public Blockquote(Parent parent) {
        super(parent, "blockquote");
    }

}
