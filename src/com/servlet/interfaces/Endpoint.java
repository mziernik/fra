package com.servlet.interfaces;

import com.user.right.UserRight;
import com.servlet.views.ViewController;
import java.lang.annotation.*;

@Inherited
@Target(value = {ElementType.TYPE, ElementType.METHOD})
@Retention(value = RetentionPolicy.RUNTIME)
public @interface Endpoint {

    public String[] url() default {}; // adresy URL, obsługiwana maska, np
    // {"url1", url1/*"}

    public String title() default "";  // tytuł strony

    public String description() default ""; // opis

    //  public String[] excludeGroups() default {}; // wykluczone grupy
    public boolean auth() default true; // strona wymaga autoryzacji

    public boolean session() default true; // sesja będzie utworzona

    public boolean logRequests() default true; // loguj żądania

    public HttpMethod[] methods() default {HttpMethod.GET, HttpMethod.POST};

    public boolean disableLogs() default false; // wylacz wszystkie logi

    public boolean ssl() default false; // strona musi być szyfrowana

    public boolean devOnly() default false; //tylko w trybie debugowania

    public String[] resources() default {}; // zasoby danej strony (pliki js, css, ResFile)

    public Class<? extends ViewController> view() default ViewController.class;

    public Class<? extends UserRight>[] rights() default {}; // wymagane uprawnienia

}
