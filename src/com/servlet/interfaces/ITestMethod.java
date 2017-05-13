package com.servlet.interfaces;

import java.lang.annotation.*;

@Target(value = {ElementType.METHOD})
@Retention(value = RetentionPolicy.RUNTIME)
public @interface ITestMethod {

    public String name() default "";  // tytuł strony

}
