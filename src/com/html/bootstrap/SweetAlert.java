package com.html.bootstrap;

import com.exceptions.ThrowableException;
import com.utils.text.StrWriter;
import com.utils.Utils;
import com.utils.Is;
import com.html.core.tag.Element;
import com.html.js.*;
import com.html.js.core.JsAction;
import com.json.JObject;
import com.resources.Res;
import java.io.IOException;

/**
 * @author Miłosz Ziernik
 * @date 24 września 2015
 * @encoding UTF-8
 */
public class SweetAlert extends JsAction {

    private final JsActions onDone = new JsActions();
    private Element tag;

    public static enum AlertType {

        error,
        warning,
        info,
        success,
        input,
        prompt;
    }
    private AlertType type;
    private Object text;
    private Object title;
    // private Boolean closeOnConfirm;
    //private Boolean closeOnCancel;
    private Boolean showCancelButton;
    private Integer timer;

    public SweetAlert(Object text) {
        this(AlertType.info, text);
    }

    public SweetAlert(AlertType type, Object text) {
        this.type = type;
        this.text = text;
        this.title = type.name();
    }

    @Override
    public void getContent(StrWriter writer) {
        JObject json = new JObject();
        json.options.javascriptMode(true).compactMode(writer.isCompact());

        if (type != null)
            json.put("type", type.name());

        if (title == null && text != null)
            json.put("title", text);
        else {

            if (text != null)
                json.put("text", text);

            if (title instanceof JsAction)
                json.put("title", title);

            json.put("title", title);
        }
        if (timer != null)
            json.put("timer", timer);

        if (showCancelButton != null)
            json.put("showCancelButton", showCancelButton);

        //  if (!onDone.isEmpty())
        //     json.put("closeOnConfirm", false);

        /* if (closeOnConfirm != null)
         json.put("closeOnConfirm", closeOnConfirm);

         if (closeOnCancel != null)
         json.put("closeOnCancel", closeOnCancel);
         */
        writer.intent().append("swal(");

        try {
            json.write(writer);
        } catch (IOException ex) {
            throw new ThrowableException(ex);
        }

        if (!onDone.isEmpty()) {
            writer.append(",").append("function(inputValue){").lineBreak();
            writer.lineBreak().intent();
            // new Call("swal.close").getContent(writer, level + 1);
            onDone.getContent(writer);
            writer.lineBreak().intent().append("}");
        }

        writer.append(");");
    }

    public SweetAlert text(Object text) {
        this.text = text;
        return this;
    }

    public SweetAlert title(Object title) {
        this.title = title;
        return this;
    }

    public SweetAlert type(AlertType type) {
        this.type = type;
        return this;
    }

    public SweetAlert timer(Integer timer) {
        this.timer = timer;
        return this;
    }

    public SweetAlert showCancelButton(Boolean showCancelButton) {
        this.showCancelButton = showCancelButton;
        return this;
    }

    /*
     public SweetAlert closeOnConfirm(Boolean closeOnConfirm) {
     this.closeOnConfirm = closeOnConfirm;
     return this;
     }

     public SweetAlert closeOnCancel(Boolean closeOnCancel) {
     this.closeOnCancel = closeOnCancel;
     return this;
     }
     */
    public SweetAlert onDone(JsAction... actions) {
        onDone.add(actions);
        if (tag != null)
            onDone.setTag(tag);
        return this;
    }

    @Override
    public JsAction setTag(Element tag) {
        link(tag, Res.sweetAlert);
        this.tag = tag;
        onDone.setTag(tag);
        return super.setTag(tag);
    }

}

/*
 var defaultParams = {
 title : '',
 text : '',
 type : null,
 allowOutsideClick : false,
 showConfirmButton : true,
 showCancelButton : false,
 closeOnConfirm : true,
 closeOnCancel : true,
 confirmButtonText : 'OK',
 confirmButtonColor : '#8CD4F5',
 cancelButtonText : 'Cancel',
 imageUrl : null,
 imageSize : null,
 timer : null,
 customClass : '',
 html : false,
 animation : true,
 allowEscapeKey : true,
 inputType : 'text',
 inputPlaceholder : '',
 inputValue : '',
 showLoaderOnConfirm : false
 };
 */
