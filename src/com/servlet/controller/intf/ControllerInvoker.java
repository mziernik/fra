package com.servlet.controller.intf;

import com.servlet.controller.Controller;
import com.servlet.requests.HttpRequest;
import com.utils.reflections.TMethod;

public interface ControllerInvoker {

    public void invoke(Controller controller, HttpRequest http, TMethod method, Object... args) throws Exception;
}
