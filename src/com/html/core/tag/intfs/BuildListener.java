package com.html.core.tag.intfs;

import com.html.core.tag.Element;

/**
 * @author Miłosz Ziernik
 * @date 26 sierpnia 2015
 * @encoding UTF-8
 */
public interface BuildListener<TTag extends Element> {

    public boolean onBeforeBuildTag(TTag tag);
}
