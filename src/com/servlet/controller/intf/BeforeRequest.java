package com.servlet.controller.intf;

import com.servlet.controller.ControllerEndpoint;
import com.utils.reflections.TMethod;

public interface BeforeRequest {

    public void call(ControllerEndpoint<?> ctrl, TMethod method) throws Exception;

}
