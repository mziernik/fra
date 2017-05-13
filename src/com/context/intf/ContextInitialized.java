package com.context.intf;

import java.lang.annotation.*;

@Target(value = {ElementType.METHOD})
@Retention(value = RetentionPolicy.RUNTIME)
public @interface ContextInitialized {

    ContextInitStage stage() default ContextInitStage.allDone;

    boolean async() default false; // tryb asynchroniczny - zadanie wykona sie w osobnym watku

    String asynThreadGroupName() default "";

    /**
     * @return błędy, które wystąpią zostaną zignorowane i nie będą miały wpływu
     * na funkcjonowanie usługi
     */
    boolean ignoreErrors() default false;

    /**
     * @return kolejność ywkonywania zdarzeń. Im wyższa wartość tym zadanie
     * zostanie wcześniej wykonane
     */
    int order() default 0;

    String[] ifAvailable() default {};
}
