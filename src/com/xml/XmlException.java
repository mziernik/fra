package com.xml;

import com.exceptions.EError;
import com.lang.LUtil;
import com.lang.LXml;
import org.xml.sax.SAXParseException;

public class XmlException extends Exception {

    public XmlException(String message) {
        super(message);
    }

    public XmlException(Exception e) {
        super(LXml.PARSE_ERROR.toString()
                + EError.exceptionToStr(e)
                + (e instanceof SAXParseException
                        ? "; " + LXml.LINE.toString() + ": " + ((SAXParseException) e).getLineNumber()
                        + ", " + LXml.COLUMN + ": " + ((SAXParseException) e).getColumnNumber()
                        : ""));
    }
}
