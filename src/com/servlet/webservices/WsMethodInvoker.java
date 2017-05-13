package com.servlet.webservices;

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;

public interface WsMethodInvoker {

    public Object invoke(Object obj, com.sun.xml.ws.api.message.Packet packet, Method method, Object... parameters)
            throws InvocationTargetException, IllegalAccessException;
}
