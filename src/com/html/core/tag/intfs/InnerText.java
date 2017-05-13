package com.html.core.tag.intfs;

import com.html.core.tag.*;
import com.utils.collections.Strings;
import java.util.LinkedList;

/**
 * @author Miłosz Ziernik
 * @date 25 sierpnia 2015
 * @encoding UTF-8
 */
public interface InnerText<TTag extends Element> extends Tag<TTag> {

    default TTag text(Object innerText) {
        new Text(this, true).setInnerText(innerText, false);
        return (TTag) this;
    }

    default TTag cdata(Object innerText) {
        new Text(this, true).setInnerText(innerText, true);
        return (TTag) this;
    }

    default TTag addText(Object innerText) {
        new Text(this, false).setInnerText(innerText, false);
        return (TTag) this;
    }

    default Strings getText() {

        Strings result = new Strings();
        result.separator(" ");

        for (Text txt : getChildren(Text.class))
            if (txt.getInnerText() != null)
                result.add(txt.getInnerText());
        return result;
    }

}
