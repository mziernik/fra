package com.html.core.tag.intfs;

import com.html.core.HtmlAttributes.HtmlAttr;
import com.html.core.tag.Element;

public interface TagChangeListener {

    default void onCreateTag(Element tag) {

    }

    default void onRemoveTag(Element tag) {

    }

    default void onAttributeAdded(Element parent, HtmlAttr<?> attribute) {

    }

    default void onAttributeChanged(Element parent, HtmlAttr<?> oldAttr, HtmlAttr<?> newAttr) {

    }

}
