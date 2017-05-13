package com.database.elements;

import java.lang.annotation.*;

/**
 * Miłosz Ziernik 2014/06/20
 */
@Target(value = {ElementType.TYPE})
@Retention(value = RetentionPolicy.RUNTIME)
public @interface JdbcDriver {

    public String value();

}
