package com.servlet.interfaces;

import com.servlet.handlers.*;
import com.servlet.requests.HttpRequest;

/**
 * *************************************************************************
 * page ************************************************************************
 */
/**
 * Umożliwia zdefiniowanie własnej sesji lub podłączenie się do już istniejącej
 */
public interface ICustomSession {

    public String getSessionId(HttpRequest request) throws Exception;
}
