package com.html.core.tag.semantic;

import com.html.core.tag.Element;
import com.html.core.tag.Tag;
import com.html.core.tag.intfs.*;

/**
 * Tag typu kontener
 *
 * @author Miłosz Ziernik
 * @date 25 sierpnia 2015
 * @encoding UTF-8
 */
public abstract class CTag<TTag extends CTag> extends Element<TTag>
        implements Container<TTag> {

    public CTag(Tag< ? extends Parent> parent, String name) {
        super(parent, name);
    }

}
