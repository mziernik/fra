package com.servlet.webservices;

import java.lang.annotation.*;
import javax.xml.ws.handler.soap.SOAPHandler;

@Retention(value = RetentionPolicy.RUNTIME)
@Target(value = {ElementType.TYPE})
public @interface IWebService {

    public String urlMapping();

    public Class<? extends SOAPHandler> handler() default SoapHandler.class;

    public Class<? extends WsMethodInvoker> invoker() default WsMethodInvoker.class;

    public boolean disabledIndevMode() default false;
}
