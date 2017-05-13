package com.servlet.webservices;

import com.mlogger.Log;
import com.mlogger.LogKind;
import com.utils.Utils;
import com.utils.Is;
import com.xml.*;
import java.io.StringWriter;
import java.util.*;
import javax.servlet.http.HttpServletRequest;
import javax.xml.namespace.QName;
import javax.xml.parsers.*;
import javax.xml.soap.*;
import javax.xml.transform.*;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.stream.StreamResult;
import javax.xml.ws.handler.*;
import javax.xml.ws.handler.soap.SOAPHandler;
import javax.xml.ws.handler.soap.SOAPMessageContext;
import org.w3c.dom.*;
import org.w3c.dom.Node;

public class SoapHandler implements HandlerResolver, SOAPHandler<SOAPMessageContext> {

    public SoapHandler() {

    }

    @Override
    public boolean handleMessage(SOAPMessageContext messageContext) {
        logSOAPMessage(messageContext, false);
        return true;
    }

    @Override
    public Set<QName> getHeaders() {
        return Collections.EMPTY_SET;
    }

    @Override
    public boolean handleFault(SOAPMessageContext messageContext) {
        logSOAPMessage(messageContext, true);
        return true;
    }

    @Override
    public void close(MessageContext context) {
    }

    public static LogKind defaultLogKind = LogKind.DEBUG;

    ;

    public static SoapMsgData logSOAPMessage(SOAPMessageContext context, boolean fault) {
        try {
            SoapMsgData data = new SoapMsgData(context);

            LogKind kind = fault ? LogKind.ERROR : defaultLogKind;

            if (fault) {

                SOAPFault sFault = context.getMessage().getSOAPBody().getFault();
                if (sFault != null && sFault.getDetail() != null) {
                    Detail sDetail = sFault.getDetail();
                    NodeList nodes = sDetail.getElementsByTagName("critical");
                    if (nodes != null && nodes.getLength() == 1) {
                        Boolean critical = Utils.strBool(nodes.item(0).getTextContent(), null);
                        if (Boolean.FALSE.equals(critical))
                            kind = LogKind.WARNING;
                    }
                }
            }

            Log log = new Log(kind).tag("SOAP");
            log.tag(Boolean.TRUE.equals(data.outbound) ? "Send"
                    : Boolean.FALSE.equals(data.outbound) ? "Receive"
                    : null);

            if (data.body != null) {
                log.tag(data.body.getLocalName());

                XML xml = new XML(data.body.getOwnerDocument());
                xml.options.header = null;

                log.details(xml.toString());

                XmlUtils.removeSoapElements(xml);

                XmlNode xBody = XmlUtils.getSoapBody(xml);

                if (xBody != null)
                    log.value(xBody.toString());

            }

            //       for (Map.Entry<String, Object> en : context.entrySet())
            //          log.attribute("SOAP", en.getKey(), en.getValue());
            log.attribute("SOAP", "Message Outbound", context.get(MessageContext.MESSAGE_OUTBOUND_PROPERTY));
            log.attribute("SOAP", "HTTP Request Method", context.get(MessageContext.HTTP_REQUEST_METHOD));
            log.attribute("SOAP", "Path Info", context.get(MessageContext.PATH_INFO));
            log.attribute("SOAP", "Query String", context.get(MessageContext.QUERY_STRING));
            log.attribute("SOAP", "HTTP Response Code", context.get(MessageContext.HTTP_RESPONSE_CODE));
            log.attribute("SOAP", "Reference Parameters", context.get(MessageContext.REFERENCE_PARAMETERS));
            log.attribute("SOAP", "WSDL Description", context.get(MessageContext.WSDL_DESCRIPTION));
            log.attribute("SOAP", "WSDL Interface", context.get(MessageContext.WSDL_INTERFACE));
            log.attribute("SOAP", "WSDL Operation", context.get(MessageContext.WSDL_OPERATION));
            log.attribute("SOAP", "WSDL Port", context.get(MessageContext.WSDL_PORT));
            log.attribute("SOAP", "WSDL Service", context.get(MessageContext.WSDL_SERVICE));

            if (data.request != null) {
                log.address(data.request.getRemoteHost());

                Enumeration<String> headerNames = data.request.getHeaderNames();
                while (headerNames.hasMoreElements()) {
                    String s = headerNames.nextElement();
                    log.attribute("Header", s, data.request.getHeader(s));
                }
            }

            log.send();

            return data;
        } catch (Exception e) {
            Log.error(e);
        }
        return null;
    }

    @Override
    public List<Handler> getHandlerChain(PortInfo portInfo) {
        List<Handler> handlerList = new ArrayList<>();
        handlerList.add(this);
        return handlerList;
    }

    public static class SoapMsgData {

        public final Throwable exception;
        public final Element body;
        public final Element header;
        //  public final SOAPBody soapBody;
        //  public final SOAPHeader soapHeader;
        public final boolean outbound;
        public final SOAPMessage message;
        public final HttpServletRequest request;

        public SoapMsgData(SOAPMessageContext context) throws TransformerException, SOAPException {

            request = (HttpServletRequest) context.get("javax.xml.ws.servlet.request");

            Object o = context.get(MessageContext.MESSAGE_OUTBOUND_PROPERTY);
            outbound = o == null ? null
                    : o instanceof Boolean ? (Boolean) o
                            : Utils.strBool(o.toString(), false);

            SOAPMessage message = null;
            Throwable exception = null;
            try {
                message = context.getMessage();
            } catch (Throwable e) {
                exception = e;
                Log.error(e);
            }
            this.exception = exception;
            this.message = message;

            Element node = null;
            if (message != null) {
                NodeList nodes = message.getSOAPBody().getChildNodes();
                for (int i = 0; i < nodes.getLength(); i++) {
                    Node item = nodes.item(i);
                    if (item.getNodeType() != Node.ELEMENT_NODE)
                        continue;
                    node = (Element) item;
                    break;
                }
            }
            body = node;

            node = null;
            if (message != null) {
                NodeList nodes = message.getSOAPHeader().getChildNodes();
                for (int i = 0; i < nodes.getLength(); i++) {
                    Node item = nodes.item(i);
                    if (item.getNodeType() != Node.ELEMENT_NODE)
                        continue;
                    node = (Element) item;
                    break;
                }
            }
            header = node;

        }

    }

}
