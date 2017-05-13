package com.servlet.interfaces;

import java.lang.annotation.*;

@Target(value = {ElementType.TYPE})
@Retention(value = RetentionPolicy.RUNTIME)
public @interface ITestClass {

    public String name() default "";  // tytuł strony
}
