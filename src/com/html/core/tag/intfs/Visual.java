package com.html.core.tag.intfs;

import com.utils.Utils;
import com.utils.Is;
import com.html.core.tag.Element;
import com.html.core.styles.Selector;
import com.html.core.tag.Tag;
import com.html.js.core.JsAction;
import com.utils.collections.Strings;
import java.util.Arrays;

/**
 * @author Miłosz Ziernik
 * @date 25 sierpnia 2015
 * @encoding UTF-8
 */
public interface Visual<TTag extends Element> extends Tag<TTag> {

    default TTag title(Object title) {
        return (TTag) ((Element) this).attrs.set("title", Utils.toString(title)).tag;
    }

    default Selector<TTag> style() {
        return ((Element) this).new Helper().style();
    }

    default TTag clsSet(String cssClass) {
        return (TTag) ((Element) this).attrs.set("class", cssClass).tag;
    }

    default Strings getClasses() {
        Strings list = new Strings().unique(true).trim(true);
        String cls = (String) ((Element) this).attrs.getValue("class");
        if (cls != null)
            list.addAll(cls.split(" "));
        return list;
    }

    default TTag cls(String cssClass) {
        Strings lst = getClasses();
        lst.add(cssClass);
        return (TTag) ((Element) this).attrs.set("class", lst.toString(" ")).tag;
    }

    /**
     * The tabindex attribute specifies the tab order of an element (when the
     * "tab" button is used for navigating).
     *
     * @param tabindex
     * @return
     */
    default TTag tabIndex(int tabindex) {
        return (TTag) ((Element) this).attrs.setNumber("tabindex", tabindex).tag;
    }

    /**
     * Specifies that an element is not yet, or is no longer, relevant
     *
     * @param hidden
     * @return
     */
    default TTag hidden(boolean hidden) {
        return (TTag) ((Element) this).attrs.setState("hidden", hidden).tag;
    }

    /**
     * Specifies a shortcut key to activate/focus an element
     *
     * @param accesskey
     * @return
     */
    default TTag acceskey(String accesskey) {
        return (TTag) ((Element) this).attrs.set("accesskey", accesskey).tag;
    }

    /**
     * Specifies whether the content of an element is editable or not
     *
     * @param contenteditable
     * @return
     */
    default TTag contenteditable(boolean contenteditable) {
        return (TTag) ((Element) this).attrs.setState("contenteditable", contenteditable).tag;
    }

    /**
     * Specifies whether an element is draggable or not
     *
     * @param draggable
     * @return
     */
    default TTag draggable(boolean draggable) {
        return (TTag) ((Element) this).attrs.setState("draggable", draggable).tag;
    }

    /**
     * Specifies whether the element is to have its spelling and grammar checked
     * or not
     *
     * @param spellcheck
     * @return
     */
    default TTag spellcheck(boolean spellcheck) {
        return (TTag) ((Element) this).attrs.setState("spellcheck", spellcheck).tag;
    }

    default TTag onClick(JsAction... onClick) {
        return (TTag) ((Element) this).attrs.setAct("onclick", onClick).tag;
    }

    default TTag onDoubleClick(JsAction... onDoubleClick) {
        return (TTag) ((Element) this).attrs.setAct("ondblclick", onDoubleClick).tag;
    }

    default TTag onKeyDown(JsAction... onKeyDown) {
        return (TTag) ((Element) this).attrs.setAct("onkeydown", onKeyDown).tag;
    }

    default TTag onKeyPress(JsAction... onKeyPress) {
        return (TTag) ((Element) this).attrs.setAct("onkeypress", onKeyPress).tag;
    }

    default TTag onKeyUp(JsAction... onKeyUp) {
        return (TTag) ((Element) this).attrs.setAct("onkeyup", onKeyUp).tag;
    }

    default TTag onWheel(JsAction... onWheel) {
        return (TTag) ((Element) this).attrs.setAct("onwheel", onWheel).tag;
    }

    default TTag onMouseDown(JsAction... onMouseDown) {
        return (TTag) ((Element) this).attrs.setAct("onmousedown", onMouseDown).tag;
    }

    default TTag onMouseMove(JsAction... onMouseMove) {
        return (TTag) ((Element) this).attrs.setAct("onmousemove", onMouseMove).tag;
    }

    default TTag onMouseOut(JsAction... onMouseOut) {
        return (TTag) ((Element) this).attrs.setAct("onmouseout", onMouseOut).tag;
    }

    default TTag onMouseOver(JsAction... onMouseOver) {
        return (TTag) ((Element) this).attrs.setAct("onmouseover", onMouseOver).tag;
    }

    default TTag onMouseUp(JsAction... onMouseUp) {
        return (TTag) ((Element) this).attrs.setAct("onmouseup", onMouseUp).tag;
    }

    default TTag onScroll(JsAction... onScroll) {
        return (TTag) ((Element) this).attrs.setAct("onscroll", onScroll).tag;
    }

    default TTag onCopy(JsAction... onCopy) {
        return (TTag) ((Element) this).attrs.setAct("oncopy", onCopy).tag;
    }

    default TTag onCut(JsAction... onCut) {
        return (TTag) ((Element) this).attrs.setAct("oncut", onCut).tag;
    }

    default TTag onPaste(JsAction... onPaste) {
        return (TTag) ((Element) this).attrs.setAct("onpaste", onPaste).tag;
    }
}
