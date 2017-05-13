package com.servlet.controller.intf;

import com.servlet.controller.ControllerEndpoint;

public interface ControllerEvent {

    public void call(ControllerEndpoint<?> ctrl) throws Exception;
;
}
