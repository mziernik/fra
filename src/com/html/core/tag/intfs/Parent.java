package com.html.core.tag.intfs;

import com.exceptions.ThrowableException;
import com.html.core.styles.Selector;
import com.html.core.tag.Element;
import com.html.core.tag.Tag;
import com.html.core.tag.semantic.CTag;
import com.html.core.tag.semantic.Style;
import com.utils.Char;
import java.util.Comparator;
import java.util.Iterator;
import java.util.LinkedList;

/**
 * Tag implementujący ten interfejs może zawierać inne tagi
 *
 * @author Miłosz Ziernik
 * @date 25 sierpnia 2015
 * @encoding UTF-8
 */
public interface Parent<TTag extends Element> extends Iterable<Element>, Tag<TTag> {

    default CTag<?> tag(String name) {
        return new Element.CustomTag((Tag<? extends Parent>) this, name);
    }

    @Override
    default Iterator<Element> iterator() {
        return ((Element) this).new Helper().iterator();
    }

    default void addTag(Tag tag) {
        getChildren().add(tag);
    }

    default <T extends Element> T tag(Class<? extends T> tag) {
        try {
            return tag.getDeclaredConstructor(Parent.class).newInstance(this);
        } catch (Exception ex) {
            throw new ThrowableException(ex);
        }
    }

    /**
     * Usuń tagi danego typu. Jeśli typ nie jest zadeklarowany (null), usuń
     * wszystko
     *
     * @param type
     * @return
     */
    default Element removeTags(Class<? extends Element> type) {
        LinkedList<Element> tags = ((Element) this).getChildren(type);
        for (Element t : tags)
            t.remove();
        return (Element) this;
    }

    default <T extends Element & InnerText & Visual> LinkedList<T> textToTags(Class<T> tagClass,
            String text) {
        return textToTags(tagClass, text, null);
    }

    default <T extends Element & InnerText & Visual> LinkedList<T> textToTags(Class<T> tagClass,
            String text, Integer spaceMargin) {

        // jesli spaceMargin == null, spacje zamienione beda na twarde
        LinkedList<T> lst = new LinkedList<>();
        if (text == null)
            return lst;

        String[] ss = text.split("\\n");
        int lineNr = 0;
        for (String st : ss) {

            T tag;
            try {
                tag = tagClass.getDeclaredConstructor(Parent.class).newInstance(this);
            } catch (Exception ex) {
                throw new ThrowableException(ex);
            }

            lst.add(tag);

            int cnt = 0;
            for (int i = 0; i < st.length(); i++) {

                boolean stop = false;

                switch (st.charAt(i)) {
                    case ' ':
                        cnt += 1;
                        break;
                    case '\t':
                        ++cnt;
                        cnt += 2;
                        break;
                    default:
                        stop = true;
                }
                if (stop)
                    break;

            }

            String space = "";

            if (cnt > 0 && spaceMargin != null && spaceMargin > 0) {
                cnt *= spaceMargin;
                tag.style().paddingLeft(cnt + "px");
            }
            if (cnt > 0 && spaceMargin == null)
                for (int i = 0; i < cnt; i++)
                    space += Char.nbsp;

            tag.text(space + st.trim());

            ++lineNr;

            //  if (breakLines && lineNr < ss.length)
            //      lst.add(tagNC("br"));
        }

        return lst;
    }

    default Selector<Style> styles(String... names) {
        Style style = this.getChildren(Style.class).peek();
        if (style == null)
            style = new Style(this);
        return style.selector(names);
    }
}
