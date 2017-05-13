package com.servlet.handlers;

import com.utils.Utils;
import com.utils.Is;

/**
 * Miłosz Ziernik 2014/01/28
 */
public class TestClass {

    public static boolean isTestMode() {
        return false;
        //return Utils.isInStackTrace(PTests.class);
    }

    protected final void assertBoolean(String name, final boolean... objects) {
        if (objects == null || objects.length == 0)
            return;
        for (int i = 0; i < objects.length; i++)
            if (!objects[i])
                throw new TestException(name + ": element " + (i + 1));
    }

    /**
     * Porównuje wszystkie obiekty ze sobą. Jeśli się różnią, generowany jest
     * wyjątek. Jeśli porónywane są obiekty numeryczne należące do różnych klas,
     * ale o tej samej wartości, to uznawane są za identyczne.
     *
     * @param name
     * @param objects
     */
    protected final void assertEquals(final String name, final Object... objects) {
        if (objects == null || objects.length == 0)
            return;

        for (int i = 0; i < objects.length; i++)
            for (int j = 0; j < objects.length; j++)
                if (i != j) {
                    Object o1 = objects[i];
                    Object o2 = objects[j];
                    if (o1 == null && o2 == null)
                        continue;

                    if (Utils.equals(o1, o2))
                        return;

                    if (o1 != null && !o1.equals(o2))
                        throw new TestException(name + ": "
                                + o1.getClass().getSimpleName()
                                + " (" + Utils.cutLongName(o1.toString(), 80,
                                        false) + ") <> "
                                + (o2 != null ? o2.getClass().getSimpleName()
                                        + " (" + Utils.cutLongName(o2.toString(), 80,
                                                false) + ")" : "null"));

                    if (o2 != null && !o2.equals(o1))
                        throw new TestException(name + ": "
                                + o2.getClass().getSimpleName()
                                + " (" + Utils.cutLongName(o2.toString(), 80,
                                        false) + ") <> "
                                + (o1 != null ? o1.getClass().getSimpleName()
                                        + " (" + Utils.cutLongName(o1.toString(), 80,
                                                false) + ")" : "null"));
                }

    }

    public static class TestException extends RuntimeException {

        public TestException(String message) {
            super(message);
        }
    }

}
