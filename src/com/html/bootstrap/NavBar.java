package com.html.bootstrap;

import com.html.core.tag.Tag;
import com.html.bootstrap.intfs.BsElement;
import com.html.bootstrap.intfs.MenuBuilder;
import com.html.core.Html;
import com.html.core.styles.FontWeight;
import com.servlet.controller.ControllerMetaData;
import com.html.core.tag.*;
import com.html.core.tag.intfs.*;
import com.html.core.tag.list.Li;
import com.html.core.tag.list.Ul;
import com.html.core.tag.semantic.Div;
import com.html.core.tag.semantic.Nav;
import com.html.menu.AbstractItem;
import com.html.menu.ControllerItem;
import com.html.menu.GroupItem;
import com.html.menu.Separator;
import com.servlet.controller.Controller;

/**
 * @author Miłosz Ziernik
 * @date 04 września 2015
 * @encoding UTF-8
 */
public class NavBar implements BsElement, Tag<Ul>, MenuBuilder {

    public final Nav main;
    public final Div container;
    public final Div collapse;
    public final Ul ul;

    public <T extends Element & Parent & Visual> NavBar(Tag<T> parent) {
        main = new Nav(parent.getElement());
        main.cls("navbar navbar-default navbar-inverse");
        container = new Div(main);
        container.cls("container-fluid");
        collapse = container.div();
        collapse.cls("collapse navbar-collapse");
        ul = collapse.ul();
        ul.cls("nav navbar-nav");
    }

    public NavBarMenuItem addItem(String caption) {
        return new NavBarMenuItem(caption);
    }

    public Li addSeparator() {
        return ul.li().cls("divider").attrs.set("role", "separator").tag;
    }

    public NavBarMenuItem addItem(Class<? extends Controller> page) {
        return new NavBarMenuItem(page);
    }

    @Override
    public Ul getElement() {
        return ul;
    }

    @Override
    public void buildMenu(GroupItem element) {
        visit(element, null);
    }

    private void visit(AbstractItem item, DropDownMenu menu) {

        if (item instanceof GroupItem) {
            GroupItem el = (GroupItem) item;

            if (item.parent != null) {
                menu = menu == null
                        ? new DropDownMenu(addItem(el.caption))
                        : new DropDownMenu(menu.addItem(el.caption));

                item.setTag(menu.ul);
            }

            for (AbstractItem it : el.children)
                visit(it, menu);
            return;
        }

        if (item instanceof Separator && menu != null)
            item.setTag(menu.addSeparator());

        if (item instanceof ControllerItem) {
            ControllerItem ci = (ControllerItem) item;

            ci.setTag(menu == null ? addItem(ci.ctrl)
                    : menu.addItem(ci.ctrl));

            Html html = ci.getTag().getHTML();

            if (html != null && html.getController() != null
                    && html.getController().getClass() == ci.ctrl)
                ((Visual) ci.getTag()).style().fontWeight(FontWeight.bold);
        }

        //NavBarMenuItem mi =  menu != null ? menu.addItem(item.caption) : addItem(item.caption);
    }

    public class NavBarMenuItem extends A {

        private NavBarMenuItem(String caption) {
            super(ul.li());
            text(caption);
            hrefVoid();
        }

        private NavBarMenuItem(Class<? extends Controller> page) {
            this(ControllerMetaData.getByClass(page).endpoint().title());
            href(page);
        }
    }

}
