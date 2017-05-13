package com.servlet.webservices;

import com.utils.Utils;
import com.utils.Is;
import com.context.AppContext;
import com.exceptions.EError;
import com.mlogger.Log;
import com.utils.collections.Params;
import javax.xml.namespace.QName;
import javax.xml.soap.*;
import javax.xml.ws.WebFault;
import javax.xml.ws.soap.SOAPFaultException;

@WebFault(name = "WSException")
public class WSException extends SOAPFaultException {

    public WSException(String message) {
        this(null, message, false, null);
    }

    public WSException(Integer code, String message) {
        this(code, message, false, null);
    }

    public WSException(Integer code, String message, boolean critical, Params details) {
        super(getSOAPFault(code, message, critical, details));
    }

    public WSException(Integer code, Throwable e) {
        this(code, e, !AppContext.releaseMode());
    }

    public WSException(Integer code, Throwable e, boolean stackTrace) {
        this(code, new EError(e).shortMessages.toString("\r\n"),
                e instanceof Exception,
                stackTrace ? new Params().add("StackTrace", EError.getStackTrace(e))
                        : null);

    }

    /*
     @Override
     public String toString() {
     return getMessage();
     }
     */
    // Utworzenie SOAPFault na podstawie tresci i numeru bledu
    private static SOAPFault getSOAPFault(Integer faultCode, String faultString, boolean critical, Params details) {
        try {
            SOAPFault fault = SOAPFactory.newInstance()
                    .createFault(faultString, faultCode != null
                            ? new QName(Integer.toString(faultCode)) : null);

            fault.addDetail().addDetailEntry(new QName("critical")).setTextContent(Boolean.toString(critical));

            if (details != null) {
                Detail detail = fault.addDetail();

                for (Params.Param p : details)
                    detail.addDetailEntry(new QName(p.name))
                            .setTextContent(Utils.toString(p.value));
            }

            return fault;
        } catch (SOAPException e) {
            Log.warning(e);
            return null;
        }
    }
}
