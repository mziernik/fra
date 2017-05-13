package com.html.core.tag.semantic;

import com.utils.text.StrWriter;
import com.html.core.styles.Selector;
import com.html.core.tag.Element;
import com.html.core.tag.intfs.*;

/**
 * @author Miłosz Ziernik
 * @date 24 sierpnia 2015
 * @encoding UTF-8
 */
public class Style extends Element<Style> implements InnerText<Style>, ScriptTag {

    public Style(Parent parent) {
        super(parent, "style");
    }

    @Override
    protected void writeContentText(StrWriter writer) {
        if (builder == null)
            return;

        writer.nextLevel(() -> {
            builder.getContent(writer);
        });

    }

    private Selector<Style> builder;

    public Selector<Style> selector(String... names) {
        if (builder == null)
            builder = new Selector<>(this);
        return builder.selector(names);
    }

}
