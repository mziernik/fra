package com.webapi.core;

import com.utils.reflections.TMethod;
import com.utils.reflections.TReflection;

public interface Invoker {

    public Object invoke(WebApi wapi, WebApiRequest request, TReflection item, Object... args) throws Exception;
}
