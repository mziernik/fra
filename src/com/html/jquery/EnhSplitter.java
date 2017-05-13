package com.html.jquery;

import com.html.core.tag.Element;
import com.html.core.tag.Head;
import com.html.core.tag.intfs.BuildListener;
import com.html.core.tag.intfs.Parent;
import com.html.js.*;
import com.json.JObject;
import com.resources.Res;

/**
 * @author Miłosz Ziernik
 * @date 20 listopada 2015
 * @encoding UTF-8
 */
public class EnhSplitter {

    //http://www.jqueryrain.com/?GZ8lwCUq
    public static enum Handle {
        default_,
        stripes,
        bar,
        block,
        dots,
        lotsofdots,
        none;

        @Override
        public String toString() {
            return name().replace("_", "");
        }

    }

    public static enum Collapse {
        left,
        right,
        up,
        down,
        none;
    }

    private final Parent tag;
    private Boolean vertical;
    private String position;
    private Integer minSize;
    private Integer maxSize;
    private Boolean invisible;
    private Handle handle; //['default'|'stripes'|'bar'|'block'|'dots'|'lotsofdots'|'none'|<user-defined>]
    ///Determines which direction the panel collapses when the handle is clicked. Setting {collapse: 'none'} leaves the handle visible while disabling its functionality, leaving the handle and hover/active effects in place for standard dragging.
    private Collapse collapse; //['left'|'right'|'up'|'down'|'none']

    // Fixes the splitter in place by disabling dragging. This option does not affect the collapse handle.
    private Boolean fixed;

    // Specifies the height of the container.
    private String height;

    private String splitterSize;

    private final JsActions onDragStart = new JsActions();
    private final JsActions onDragEnd = new JsActions();
    private final JsActions onDrag = new JsActions();
    public final JObject json = new JObject();

    public EnhSplitter position(String position) {
        this.position = position;
        return this;
    }

    public EnhSplitter vertical(boolean vertical) {
        this.vertical = vertical;
        return this;
    }

    public EnhSplitter invisible(Boolean invisible) {
        this.invisible = invisible;
        return this;
    }

    public EnhSplitter handle(Handle handle) {
        this.handle = handle;
        return this;
    }

    public EnhSplitter collapse(Collapse collapse) {
        this.collapse = collapse;
        return this;
    }

    public EnhSplitter height(String height) {
        this.height = height;
        return this;
    }

    public EnhSplitter fixed(Boolean fixed) {
        this.fixed = fixed;
        return this;
    }

    public EnhSplitter minSize(Integer minSize) {
        this.minSize = minSize;
        return this;
    }

    public EnhSplitter maxSize(Integer maxSize) {
        this.maxSize = maxSize;
        return this;
    }

    public EnhSplitter splitterSize(String splitterSize) {
        this.splitterSize = splitterSize;
        return this;
    }

    public EnhSplitter onDrag(JsActions actions) {
        onDrag.add(actions);
        return this;
    }

    public EnhSplitter onDragEnd(JsActions actions) {
        onDragEnd.add(actions);
        return this;
    }

    public EnhSplitter onDragStart(JsActions actions) {
        onDragStart.add(actions);
        return this;
    }

    public EnhSplitter(Parent tag) {
        this.tag = tag;
        tag.getHTML().head.link(Res.enhsplitter);

        tag.getHTML().head.node.addBuildListener(new BuildListener() {
            @Override
            public boolean onBeforeBuildTag(Element ttag) {

                Head head = (Head) ttag;

                Function funct = (Function) head.properties.get("EnhSplitterScript");

                if (funct == null) {
                    funct = new Function(null, "$");
                    head.properties.put("EnhSplitterScript", funct);
                    head.script(new Call("jQuery", funct));
                }

                if (vertical != null)
                    json.put("vertical", vertical);

                if (position != null)
                    json.put("position", position);

                if (minSize != null)
                    json.put("minSize", minSize);

                if (maxSize != null)
                    json.put("maxSize", maxSize);

                if (invisible != null)
                    json.put("invisible", invisible);

                if (handle != null)
                    json.put("handle", handle.toString());

                if (collapse != null)
                    json.put("collapse", collapse.name());

                if (fixed != null)
                    json.put("fixed", fixed);

                if (height != null)
                    json.put("height", height);

                if (splitterSize != null)
                    json.put("splitterSize", splitterSize);

                if (!onDragStart.isEmpty())
                    json.put("onDragStart", onDragStart.toString());

                if (!onDrag.isEmpty())
                    json.put("onDrag", onDrag.toString());

                if (!onDragEnd.isEmpty())
                    json.put("onDragEnd", onDragEnd.toString());

                funct.body(
                        new Call("$", "#" + tag.getId(true))
                                .call("enhsplitter", json)
                );

                return true;
            }
        });
    }

}
