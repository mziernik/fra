package com.config.engine.interfaces;

import com.config.engine.ConfigNode;
import com.config.engine.HConfig;
import com.intf.BoolAuto;
import com.user.right.UserRight;
import java.lang.annotation.*;

@Target(value = {ElementType.FIELD, ElementType.TYPE})
@Retention(value = RetentionPolicy.RUNTIME)
public @interface Cfg {

    public Class<? extends ConfigNode> parent() default HConfig.class;

    public String orderName() default ""; // nazwa, na podstawie ktorej odbywa sie sortowanie

    public BoolAuto externalDB() default BoolAuto.AUTO; // czy wartość ma być odczytywana z zewnętrznej bazy danych (jesli istnieje)

    public BoolAuto allowChangeDefault() default BoolAuto.AUTO; // zezwolenie na zmienę domyslnej wartości (np z pliku conf

    public Class<? extends UserRight>[] rights() default {}; // role, które uprawniają do wyświetlenia/modyfikacji wartości
}
