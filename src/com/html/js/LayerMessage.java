package com.html.js;

import com.html.js.core.JsAction;
import com.utils.text.StrWriter;
import com.html.core.Html;
import com.html.core.tag.Element;
import com.html.modules.LayerDialog.MessageType;
import com.json.Escape;
import com.resources.Res;
import com.servlet.controller.Page;
import java.util.LinkedList;
import java.util.List;

@Deprecated
public class LayerMessage extends JsAction {

    private String varName = "layer";
    private final MessageType type;
    private final JsAction message;
    private Html extraHtml;
    private JsAction onClose;
    private final List<Btn> buttons = new LinkedList<>();

    private class Btn {

        final String caption;
        final JsActions onClick;

        public Btn(String caption, JsAction... onClick) {
            this.caption = caption;
            this.onClick = new JsActions(onClick);
        }

    }

    public LayerMessage(MessageType type, String message) {
        this.type = type;
        this.message = new Eval(Escape.js(message));
    }

    public LayerMessage(MessageType type, JsAction message) {
        this.type = type;
        this.message = message;
    }

    public LayerMessage(String message) {
        this(MessageType.info, message);
    }

    public LayerMessage(JsAction message) {
        this(MessageType.info, message);
    }

    public LayerMessage setExtraHtml(Html extraHtml) {
        this.extraHtml = extraHtml;
        return this;
    }

    public LayerMessage setVarName(String varName) {
        this.varName = varName;
        return this;
    }

    public LayerMessage onClose(JsAction onClose) {
        this.onClose = onClose;
        return this;
    }

    public LayerMessage addButton(String caption, JsAction... onClick) {
        buttons.add(new Btn(caption, onClick));
        return this;
    }

    @Override
    public void getContent(StrWriter writer) {

        writer.append("var ")
                .append(varName)
                .append(" = new top.Layer(null, null);\n");

        if (!buttons.isEmpty()) {
            for (Btn btn : buttons) {

                writer.append(varName).append(".addButton(");
                escape(writer, btn.caption);
                if (!btn.onClick.isEmpty()) {
                    writer.append(", function(){\n");
                    btn.onClick.getContent(writer);
                    writer.append("\n}");
                }

                writer.append(");").append("\n");

            }
            writer.append(varName).append(".addCloseButton();\n");

        }

        if (onClose != null) {
            writer.append(varName)
                    .append(".onClose = function(){");
            onClose.getContent(writer);
            writer.append("};\n");
        }
        String url = Page.getUrl("/res/img/" + type.name() + ".png").toString();
        writer.append(varName).append(".message('").append(url).append("', ");

        Escape.js(type.label, writer).append(", ");

        message.getContent(writer);

        if (extraHtml != null) {
            writer.append(", ");
            Escape.js(extraHtml.toString(), writer);
        }

        writer.append(");");
    }

    @Override
    public JsAction setTag(Element tag) {

        if (tag == null)
            return this;

        link(tag, Res.layer);
        if (message != null)
            message.setTag(tag);

        if (onClose != null)
            onClose.setTag(tag);

        for (Btn btn : buttons)
            btn.onClick.setTag(tag);

        return this;
    }

}
