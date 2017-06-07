package com.webapi.core;

import com.user.right.UserRight;
import java.lang.annotation.ElementType;
import java.lang.annotation.Inherited;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

@Inherited
@Target(value = {ElementType.METHOD, ElementType.FIELD})
@Retention(value = RetentionPolicy.RUNTIME)
public @interface WebApiEndpoint {

    String name() default "";

    String description() default "";

    @Deprecated
    DataType_old dataType() default DataType_old.NONE;

    Class<? extends UserRight>[] rights() default {};

    int hash() default 0;

    boolean async() default false;

    boolean cancelable() default false;

    boolean auth() default true; // Czy wymagana autoryzacji

    WebApiChannelType channel() default WebApiChannelType.BOOTH;

    int version() default 0; //ToDo: implementacja wersji

}
