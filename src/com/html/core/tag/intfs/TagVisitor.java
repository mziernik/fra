/*
 */
package com.html.core.tag.intfs;

import com.html.core.tag.Element;

/**
 *
 * @author user
 */
public interface TagVisitor {

    /**
     * Zwraca true jeśli visitor ma się dalej zgłedbiać w tagi danej gałęzi
     *
     * @param tag
     * @return
     */
    public boolean visitTag(Element tag);

}
