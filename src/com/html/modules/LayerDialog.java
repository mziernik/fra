package com.html.modules;

import com.resources.Res;
import com.resources.core.html.ImgFile;
import com.servlet.controller.Page;

@Deprecated
public class LayerDialog extends WindowLayer {

    private final MessageType type;

    public static enum MessageType {

        info("Informacja", Res.info48png),
        warning("Ostrzeżenie", Res.warning48png),
        error("Błąd", Res.error48png);

        public final String label;
        public final ImgFile img;

        private MessageType(String label, ImgFile img) {
            this.label = label;
            this.img = img;
        }

    }

    public LayerDialog(Page page) {
        this(page, MessageType.info);
    }

    public LayerDialog(Page page, MessageType type) {
        super(page);
        this.type = type;

        caption = type.label;
        setIcon(page.getAbsoluteURL(type.img.href.toString()), 48, 48);

        page.body.style().minWidth("300px")
                .lineHeight("1.5em")
                .minHeight("46px")
                .fontFamilySans()
                .fontSize("9pt")
                .padding(null);

    }

}
