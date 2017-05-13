package com.config.engine.cell.select;

import com.intf.callable.Callable;
import com.intf.runnable.Runnable1;

import java.util.*;

public class CcSelectText extends CcSelect<String, String> {

    @FunctionalInterface
    public static interface EnumCaption<E extends Enum> {

        public String geCaption(E value);
    }

    public CcSelectText(Callable<Map<String, String>> enumerate, CharSequence name) {
        super(String.class, name);
        enumerateSimple(enumerate);
    }

    public CcSelectText(Runnable1<LinkedHashMap<String, String>> enumerate, CharSequence name) {
        super(String.class, name);
        enumerateSimple(enumerate);
    }

}
