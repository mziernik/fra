package com.html.bootstrap;

import com.exceptions.CoreException;
import com.html.bootstrap.intfs.BsElement;
import com.html.core.HtmlAttributes;
import com.servlet.controller.ControllerMetaData;
import com.html.core.tag.A;
import com.html.core.tag.Tag;
import com.html.core.tag.form.Button;
import com.html.core.tag.intfs.*;
import com.html.core.tag.list.Li;
import com.html.core.tag.list.Ul;
import com.resources.Res;
import com.servlet.controller.Controller;
import com.utils.collections.Strings;

/**
 * @author Miłosz Ziernik
 * @date 03 września 2015
 * @encoding UTF-8
 */
public class DropDownMenu implements BsElement {

    public final Ul ul;
    final Parent base;

    public DropDownMenu(DropDownMenuItem menuItem) {
        this(menuItem, true);
        base.getParent().asVisual().cls("dropdown-submenu");
    }

    public DropDownMenu(NavBar.NavBarMenuItem item) {
        this(item, false);
        item.cls("dropdown");

    }

    public DropDownMenu(Button button) {
        this(button, false);
    }

    private DropDownMenu(final Parent base, boolean isSubMenu) {
        this.base = base;
        base.getHTML().head.link(Res.bootstrap);
        ul = new Ul((Parent) base.getParent());
        //  ul.attr("role", "menu");
        ul.cls("dropdown-menu");
        // ul.aria("labelledby", base.getId(true));

        //  base.aria("haspopup", "true");
        if (!isSubMenu) {
            base.asVisual().cls("dropdown-toggle")
                    .data("toggle", "dropdown");
            base.asContainer()
                    .span()
                    .cls("caret")
                    .style().marginLeft("6px");

            Strings classes = base.getParent().asVisual().getClasses();

            if (!classes.contains("dropdown") && !classes.contains("dropup"))
                base.getParent().asVisual().cls("dropdown");

        }

    }

    public DropDownMenuItem addItem(String caption) {
        return new DropDownMenuItem(caption);
    }

    public Li addSeparator() {
        return ul.li().cls("divider").attrs.set("role", "separator").tag;
    }

    public DropDownMenuItem addItem(Class<? extends Controller> page) {
        return new DropDownMenuItem(page);
    }

    public class DropDownMenuItem extends A {

        private DropDownMenuItem(String caption) {
            super(ul.li());
            text(caption);
            hrefVoid();
        }

        private DropDownMenuItem(Class<? extends Controller> page) {
            this(ControllerMetaData.getByClass(page).endpoint().title());
            href(page);
        }
    }

}
