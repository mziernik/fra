package com.servlet.interfaces;

import java.lang.annotation.*;

@Target(value = {ElementType.TYPE})
@Retention(value = RetentionPolicy.RUNTIME)
public @interface IServlet {

    public String[] names();

    //czy uwzględniać podkatalogi ścieżki
    public boolean includeSubTree() default false;
}
