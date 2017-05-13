package com.user;

import java.lang.annotation.*;

@Target(value = {ElementType.FIELD})
@Retention(value = RetentionPolicy.RUNTIME)
public @interface UserDataField {

    public String title();

    public String key() default ""; // l;ucz pola (identyfikator). Jeśli jest pusty, brana jest pod uwage nazwa pola

    public boolean required() default false;

    public String htmlInputType() default "text";

    public String databaseColumName() default "";

    public boolean password() default false;

    public String group() default ""; // nazwa grupy (strona edycji użytkownika)

}
