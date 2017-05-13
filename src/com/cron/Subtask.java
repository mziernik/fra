package com.cron;

import java.lang.annotation.*;

@Inherited
@Target(value = {ElementType.TYPE, ElementType.METHOD})
@Retention(value = RetentionPolicy.RUNTIME)
public @interface Subtask {

    public String title();  // tytuł strony

    public int order() default 0;  // java nie pamięta kolejności metod, stąd warto ją zadeklarować

}
