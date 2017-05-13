package com.servlet.websocket;

import java.lang.annotation.*;

@Target(value = ElementType.TYPE)
@Retention(value = RetentionPolicy.RUNTIME)
public @interface WebSocketEndpoint {

    public String url();

}
