package com.html.modules;

import com.xml.*;
import com.utils.StrUtils;
import com.html.core.styles.TextDecoration;
import com.html.core.tag.intfs.Parent;
import com.html.core.tag.semantic.CTag;
import com.html.core.tag.semantic.Div;
import com.xml.elements.XmlComment;
import java.io.IOException;

public class XmlToHtml extends Div {

    private void enumXmlToHtml(XmlNode node, CTag tag, int level) throws XmlException {
        Div div = tag.div();

        /*
         div.span().text(Node.nbsp(level * 4));

         div.span().text("<" + node.getName() + ">").cls("xnode");

         String txt = node.getStr("");

         Tag span = div.span().cls("xval");
         span.text(XML.escape(txt, true));

         if (span.getText() != null && !span.getText().equals(txt)) {
         span.title(txt);
         span.style().textDecoration(TextDecoration.underline);
         }

         for (XmlElement nd : node.getElements()) {
         if (nd instanceof XmlComment) {
         XmlComment cn = (XmlComment) nd;
         Node dd = div.div();
         dd.span().text(Node.nbsp((level + 1) * 4));
         dd.span().text("<!-- " + cn.getText() + " -->").cls("xcomm");
         }
         if (nd instanceof XmlNode)
         enumXmlToHtml((XmlNode) nd, div, level + 1);
         }

         if (!node.getNodes().isEmpty())
         div.span().text(Node.nbsp(level * 4));

         div.span().text("</" + node.getName() + ">").cls("xnode");
         */
    }

    public XmlToHtml(Parent parent) {
        super(parent);
        /*
         parent.builder.html.head.styles(".xnode")
         .color("purple");
         parent.builder.html.head.styles(".xval")
         .fontWeight("bold");
         parent.builder.html.head.styles(".xcomm")
         .color("#1B694D");

         style().fontFamilyMonospace()
         .fontSize("10pt"); */
    }

    public void build(XmlNode node) throws IOException, XmlException {
        enumXmlToHtml(node, this, 0);
    }

    public void build(XML xml) throws IOException, XmlException {
        Div div = this.div();
        //   div.text(xml.xmlHeader);
        div.style().color("#888");
        enumXmlToHtml(xml, this, 0);
    }
}
