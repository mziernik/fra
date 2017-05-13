package com.servlet.controller.intf;

import com.servlet.controller.Controller;
import com.servlet.controller.ControllerEndpoint;
import com.utils.reflections.TMethod;

public interface AfterRequest {

    public void call(ControllerEndpoint<? extends Controller> ctrl, TMethod method, Throwable err) throws Exception;
}
