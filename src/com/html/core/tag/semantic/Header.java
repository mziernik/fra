package com.html.core.tag.semantic;

import com.html.core.tag.intfs.Parent;

/**
 * The <header> element represents a container for introductory content or a set
 * of navigational links.
 *
 * @author Miłosz Ziernik
 * @date 24 sierpnia 2015
 * @encoding UTF-8
 */
public class Header extends CTag<Header> {

    public Header(Parent parent) {
        super(parent, "header");
    }

}
