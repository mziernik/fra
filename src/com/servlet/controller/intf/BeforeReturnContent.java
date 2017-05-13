package com.servlet.controller.intf;

import com.servlet.controller.ControllerEndpoint;
import com.servlet.requests.HttpRequest;
import com.utils.reflections.TMethod;
import java.io.IOException;

public interface BeforeReturnContent {

    public void call(ControllerEndpoint<?> ctrl, HttpRequest http) throws IOException;

}
