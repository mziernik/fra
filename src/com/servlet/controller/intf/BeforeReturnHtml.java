package com.servlet.controller.intf;

import com.html.core.tag.Element;
import com.servlet.controller.ControllerEndpoint;
import com.utils.reflections.TMethod;

public interface BeforeReturnHtml {

    public boolean call(ControllerEndpoint<?> ctrl, Element tag, int status) throws Exception;
}
