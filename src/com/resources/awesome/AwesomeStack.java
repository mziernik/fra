package com.resources.awesome;

import com.html.core.tag.Element;
import com.html.core.tag.intfs.Parent;
import com.html.core.tag.intfs.Visual;

/**
 * @author Miłosz Ziernik
 * @date 02 września 2015
 * @encoding UTF-8
 */
public class AwesomeStack extends Element<AwesomeStack>
        implements Parent<AwesomeStack>, Visual<AwesomeStack> {

    FaSize size;

    public AwesomeStack(Parent parent, FaSize size) {
        super(parent, "span");
        this.size = size;
        String ssize = size != null ? size.value : "lg";
        cls("fa-stack fa-" + ssize);
    }

    public AwesomeIcon icon(Awesome icon, FaSize size) {
        return new AwesomeIcon(this, icon, size);
    }

}
